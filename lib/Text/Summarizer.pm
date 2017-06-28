package Text::Summarizer;
use v5.10;
use strict;
use warnings;
use utf8;
use Lingua::Sentence;
use Moo;
use Types::Standard qw/ Ref Str Int /;
use List::AllUtils qw/ max min sum /;
use Data::Dumper;



has permanent_path => (
	is  => 'ro',
	isa => Str,
	default => 'words/permanent.stop',
);

has stopwords_path => (
	is  => 'ro',
	isa => Str,
	default => 'words/stopwords.stop',
);

has watchlist_path => (
	is  => 'ro',
	isa => Str,
	default => 'words/watchlist.stop'
);

has articles_path => (
	is => 'ro',
	isa => Str,
	default => 'articles/*'
);

has watch_count => (
	is => 'rwp',
	isa => Int,
	default => 0,
);

has watch_coef => (
	is => 'rwp',
	isa => Int,
	default => 30,
);

has watchlist => (
	is => 'rwp',
	isa => Ref['HASH'],
	builder => 'load_watchlist',
	lazy => 1,
);

has stopwords => (
	is => 'rwp',
	isa => Ref['HASH'],
	builder => 'load_stopwords',
	lazy => 1,
);

has full_text => (
	is => 'rwp',
	isa => Str,
);

has multi_str => (
	is => 'rwp',
	isa => Str,
);

has sentences => (
	is => 'rwp',
	isa => Ref['ARRAY'],
);

has word_list => (
	is => 'rwp',
	isa => Ref['ARRAY'],
);

has freq_hash => (
	is => 'rwp',
	isa => Ref['HASH'],
);

has cluster_hash => (
	is => 'rwp',
	isa => Ref['HASH'],
);

has phrase_hash => (
	is => 'rwp',
	isa => Ref['HASH'],
);

has sigma_hash => (
	is => 'rwp',
	isa => Ref['HASH'],
);

has inter_hash => (
	is => 'rwp',
	isa => Ref['HASH'],
);

has max_word => (
	is => 'rwp',
	isa => Int,
	default => 0,
);

has max_score => (
	is => 'rwp',
	isa => Int,
	default => 0,
);



sub load_watchlist {
	my $self = shift;
	my %watch_list;

	open( my $watchlist_file, '<', $self->watchlist_path )
		or die "Can't open " . $self->watchlist_path . ": $!";

	for (<$watchlist_file>) {
		$watch_list{$1} = $2 if m/\s*(\w+) \| \s*(\d+)/;
	}
	close $watchlist_file;

	$self->_set_watch_count( sum values %watch_list // 0 );  #counts the total number of watch_words ever collected
	$self->_set_max_word( max keys %watch_list // 0 );  #generates the length of the longest word collected
	$self->_set_max_score( max map { length } values %watch_list // 0 );  #generates the length of the longest score collected

	return \%watch_list;
}

sub load_stopwords {
	my $self = shift;
	my %stop_words;

	open( my $permanent_file, '<', $self->permanent_path )
 		or die "Can't open " . $self->permanent_path . ": $!";
	open( my $stopwords_file, '<', $self->stopwords_path )
		or die "Can't open " . $self->stopwords_path . ": $!";

	chomp xor $stop_words{ $_ } = 1 for (<$permanent_file>);
	chomp xor $stop_words{ $_ } = 1 for (<$stopwords_file>);

	close $permanent_file;
	close $stopwords_file;

	return \%stop_words;
}



sub scan_file {
	my ($self, $filepath) = @_;

	open( my $file, "<", $filepath )
		or die "Can't open $filepath: $!";

	say "\nAnalyzing file $filepath";
	$self->grow_watchlist( $file );
	$self->grow_stopwords( $file );
	$self->store_watchlist;
	$self->store_stoplist;

	return $self;

}

sub scan_all {
	my $self = shift;

	$self->scan_file( $_ ) foreach glob($self->articles_path);

	return $self;
}

sub summarize_file {
	my ($self, $filepath) = @_;

	open( my $file, "<", $filepath )
		or die "Can't open $filepath: $!";

	say "\nSummary of file $filepath";
	$self->split( $file );
	$self->analyze_frequency;
	$self->analyze_clusters;
	$self->analyze_phrases;
	$self->pretty_printer;

	return $self;
}

sub summarize_all {
	my $self = shift;

	$self->summarize_file( $_ ) foreach glob($self->articles_path);

	return $self;
}



sub grow_watchlist {
	my ($self, $file) = @_;

	for (<$file>) {
		while ( m/ ([A-Za-z]+ (?:['’][A-Za-z]+|[[A-Za-z]-[sS]])* ) /xg ) {
			my $word = lc $1;
			$self->watchlist->{$word}++ unless ( exists $self->stopwords->{$word} );
		}
	}

	$self->_set_watch_count( sum values %{$self->watchlist} // 0 );  #counts the total number of watch_words ever collected
	$self->_set_max_word( max map { length } keys %{$self->watchlist} );  #generates the length of the longest word collected
	$self->_set_max_score( max map { length } values %{$self->watchlist} );  #generates the length of the longest score collected

	return $self;
}

sub store_watchlist {
	my $self = shift;

	my @printlist = sort { $self->watchlist->{$b} <=> $self->watchlist->{$a} } keys %{$self->watchlist};

	open( my $watchlist_file, ">", $self->watchlist_path )
		or die "Can't open $self->watchlist_path: $!";

	my $string = "\%" . $self->max_word . "s | \%" . $self->max_score . "s\n";
	printf $watchlist_file $string x @printlist, map { ($_ => $self->watchlist->{$_}) } @printlist;

	close $watchlist_file;

	return $self;
}

sub grow_stopwords {
	my ( $self, $file ) = @_;
	my ( $watch_factor, $watch_length, $avgfreq );

	$watch_length = scalar keys %{$self->watchlist}; #total number of words in the WATCH_LIST
	$avgfreq = $watch_length ? $self->watch_count / $watch_length : 0; #average frequency of words in WATCH_LIST

	my ($low, $lower, $upper, $high) = ($avgfreq, 0, 0, $avgfreq);
	for my $score (values %{$self->watchlist}) {
		$score > $avgfreq ? $upper += $score : $lower += $score;
		$high = max( $score => $high );
		$low  = min( $score => $low  );
	}

	my $normal    = ($watch_length / 2);  #normalization scalar
	my $whisker   = (3 * $upper - $lower) / (2 * $normal);  #upper whisker
	$watch_factor = $whisker * ($self->watch_coef / log $self->watch_count);  #low-pass threshold

	for (keys %{$self->watchlist}) {
		$self->stopwords->{$_} = 1 if $self->watchlist->{$_} > $watch_factor;
	}


	$upper /=  $normal;
	$lower /=  $normal;
	say "lower = $lower; mid = $avgfreq; upper = $upper";
	say "upper whisker = $whisker";
	say "avg freq = $avgfreq";
	say "factor = $watch_factor";
}

sub store_stoplist {
	my $self = shift;

	open( my $stopwords_file, ">", $self->stopwords_path)
		or die "Can't open $self->stopwords_file: $!";
	print $stopwords_file "$_\n" for sort keys %{$self->stopwords};
	close $stopwords_file;

	return $self;
}



sub split {
	my ( $self, $file ) = @_;

	$self->_set_full_text( join "" => map { chomp; $_ } <$file> );

	#module for seperating sentences
	my $splitter = Lingua::Sentence->new("en");
	my $multistring = lc $splitter->split( $self->full_text );

	$multistring =~ s/(\.”|")\s/$1\n/g;
		#manual split — splitter does not work on infix periods (within quotations)
	my @sentences = split /\n/, $multistring;
		#create array of sentences
	my @splitwords = split /\s+|\n+|—|–/, $multistring;
		#create array of every word in order

	$self->_set_multi_str( $multistring );
	$self->_set_sentences( \@sentences  );
	$self->_set_word_list( \@splitwords );

	return $self;
}



sub analyze_frequency {
	my $self = shift;

	my $wordcount = scalar @{$self->word_list};  #counts the total words in the article
	my $min_length = 3;
	my %frequency;
	for (@{$self->word_list}) {
		if ($_ !~ /\A \W+ \Z/x) {
			s/ [^A-Za-z0-9]+ s? \Z //gx;
			$frequency{$_}++ if length $_ >= $min_length and not $self->stopwords->{$_};
		}
	}
	my $min_freq = int($wordcount*40/10000) or 1;
	grep { delete $frequency{$_} if $frequency{$_} < $min_freq } keys %frequency;
		#remove words that appear less than the $min_freq (defaults to 1)

	$self->_set_freq_hash( \%frequency );

	return $self;
}



sub analyze_clusters {
	my $self = shift;

	my $cluster_count;
	my %cluster_hash;
	for my $index (0..scalar @{$self->sentences} - 1) {
		my @sen_words = split /[^A-Za-z'’\-]+/, $self->sentences->[$index];

		for my $pos (0..scalar @sen_words - 1) {
			$cluster_count++;
			for my $f_word (keys %{$self->freq_hash}) {
				if (($sen_words[$pos]) =~ /\A$f_word\Z/) {
					my @array;

					push @array, $_ for (@{$cluster_hash{$f_word}}, [$index, $pos, $cluster_count]);
					$cluster_hash{$f_word} = \@array;
				}
			}
		}
	}
	$self->_set_cluster_hash( \%cluster_hash );


	my $squaresum;
	my $sum;
	my %sigma_hash;
	for my $f_word (keys %{$self->cluster_hash}) {
		for my $f_vector (@{$self->cluster_hash->{$f_word}}) {
			$squaresum += $$f_vector[2]**2;
			$sum += $$f_vector[2];
		}
		my $sigma = sqrt( ($squaresum - $sum**2 / $cluster_count) / $cluster_count );	#pop. std. deviation
		$sigma_hash{$f_word} = int( $sigma / 20 );
		$sigma_hash{$_} = $sigma_hash{$_} // 0 for keys %{$self->freq_hash};
	}
	$self->_set_sigma_hash( \%sigma_hash );

	return $self;
}



sub analyze_phrases {
	my $self = shift;

	my $size = 3;
	my @all_words = split /[^A-Za-z'’\-]+/, $self->multi_str;
	my %phrase_hash;
	for my $f_word (keys %{$self->cluster_hash}) {
		for my $f_vector (@{$self->cluster_hash->{$f_word}}) {
			my $index = $$f_vector[2] - 1;
			my @array;
			my @phrases = grep { !$self->stopwords->{$_} if $_ } @all_words[ ($index - $size) .. ($index + $size) ];
			push @array, $_ for ( @{$phrase_hash{$f_word}}, \@phrases );
			$phrase_hash{$f_word} = \@array;
		}
	}
	$self->_set_phrase_hash( \%phrase_hash );

	my %inter_hash;
	KEYWORD: for my $f_word (keys %{$self->phrase_hash}) {
		OUTER: for ( my $o = 0; $o < scalar @{$self->phrase_hash->{$f_word}}; $o++ ) {
			INNER: for ( my $i = $o + 1; $i < scalar @{$self->phrase_hash->{$f_word}}; $i++ ) {
				my $outer = @{$self->phrase_hash->{$f_word}}[$o];
				my $inner = @{$self->phrase_hash->{$f_word}}[$i];
				grep { $inter_hash{$_}++ if /\w+( \w+)+/ } intersect( $outer, $inner );
					#add phrase if it contains multiple words
			}
		}
	}
	$self->_set_inter_hash( \%inter_hash );

	return $self;
}



sub intersect {
	my ($Lref, $Rref) = (@_);

	my @Llist = @$Lref;
	my @Rlist = @$Rref;

	my @intersection;
	PHRASE: for ( my $o = -1; $o < scalar @Rlist; $o++ ) {
		my @phrase;

		RWORD: for ( my $i = $o + 1; $i < scalar @Rlist; $i++ ) {
			next PHRASE unless contains( \@Llist => [ @phrase, $Rlist[$i] ] );
			push @phrase => $Rlist[$i];
		}
		
		push @intersection, join( " " => @phrase );
	}

	return @intersection;
}



sub contains {
	my ($Lref, $Rref) = @_;
	my @Llist = @$Lref;
	my @Rlist = @$Rref;

	my $match;
	LWORD: for ( my $o = 0; $o < scalar @Llist; $o++ ) {
		$match = 0;

		RWORD: for ( my $i = 0; $i < scalar @Rlist; $i++ ) {
			next LWORD unless defined $Llist[$o+$i] and $Llist[$o+$i] eq $Rlist[$i];

			$DB::single = 1;

			$match++;
		}

		last LWORD;
	}

	return $match;
}



sub pretty_printer {
	my $self = shift;

	say "PHRASES:";
	my %inter_sorted;
	for my $phrase (sort { $self->inter_hash->{$b} <=> $self->inter_hash->{$a} } keys %{$self->inter_hash}) {
		$inter_sorted{$_} += $self->inter_hash->{$phrase} for split " ", $phrase;
		say "\t$phrase => " . $self->inter_hash->{$phrase};
	} 
	say "\n";

	my %sort_list;
	for (keys %{$self->freq_hash}) {
		$sort_list{$_} += $self->freq_hash->{$_} // 0;
		$sort_list{$_} += $self->sigma_hash->{$_} // 0;
		$sort_list{$_} += $inter_sorted{$_} // 0;
	}

	say "WORDS:";
	my $string = "%" . $self->max_word . "s|%s%s%s\n";
	printf( $string, ( $_,"-"x$self->freq_hash->{$_}, "+"x$self->sigma_hash->{$_}, "-"x($inter_sorted{$_} // 0) ))
		for sort { $sort_list{$b} <=> $sort_list{$a} } keys %sort_list;
	say "";

	return $self;
}




1;




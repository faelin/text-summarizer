#!/usr/bin/perl

package Text::Summarizer;

use v5.10;
use strict;
use warnings;
use utf8;
use Lingua::Sentence;
use Moo;
use Types::Standard qw/ Ref Str Int /;
use List::AllUtils qw/ max /;
use List::Compare;
use Data::Dumper::Simple;



has permanent_path => (
	is  => 'ro',
	isa => Str,
	default => 'words/permanent.txt',
);

has stopwords_path => (
	is  => 'ro',
	isa => Str,
	default => 'words/stopwords.log',
);

has watchlist_path => (
	is  => 'ro',
	isa => Str,
	default => 'words/watchlist.log'
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
	is => 'rwp'.
	isa => Ref['HASH'],
	builder => 'load_watchlist',
);

has stopwords => (
	is => 'rwp'.
	isa => Ref['HASH'],
	builder => 'load_stopwords',
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
		or die "Can't open $self->watchlist_path: $!";

	for (<$watchlist_file>) {
		$watch_list{$1} = $2 if m/\s*(\w+) \| \s*(\d+)/;
	}
	close $watchlist_file;

	$self->watch_count = sum( values %watch_list );  #counts the total number of watch_words ever collected
	$self->max_word    = max( keys %watch_list );  #generates the length of the longest word/score collected
	$self->max_score   = max( map { length } values %watch_list  );

	return \%watch_list;
}

sub load_stopwords {
	my $self = shift;
	my %stop_words;

	open( my $permanent_file, '<', $self->permanent_path )
 		or die "Can't open $self->permanent_path: $!";
	open( my $stopwords_file, '<', $self->stopwords_path )
		or die "Can't open $self->stopwords_path: $!";

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

	$self->split( $file );
	$self->analyze_frequency;
	$self->analyze_clusters;
	$self->analyze_phrases;
	$self->pretty_printer;

	return $self;
}

sub summarize_all {
	my $self = shift;

	$self->process_file( $_ ) foreach glob($self->articles_path);

	return $self;
}



sub grow_watchlist {
	my ($self, $file) = @_;

	for (<$file>) {
		while ( m/ ([A-Za-z]+ (?:['’][A-Za-z]+|[[A-Za-z]-[sS]])* ) /xg ) {
			my $word = lc $1;
			$self->watch_list->{$word}++ unless ( exists $self->stop_words->{$word} );
		}
	}

	$self->watch_count = sum( values %{$self->watch_list} );  #counts the total number of watch_words ever collected
	$self->max_word  = max( keys %{$self->watch_list} );  #generates the length of the longest word/score collected
	$self->max_score = max( map { length } values %{$self->watch_list} );

	return $self;
}

sub store_watchlist {
	my $self = shift;

	my @printlist = sort { $self->watch_list->{$b} <=> $self->watch_list->{$a} } keys %{$self->watch_list};

	open( my $watchlist_file, ">", $self->watchlist_path )
		or die "Can't open $self->watchlist_path: $!";
	printf $watchlist_file "\%${$self->maxword}s | \%${$self->maxnum}s\n"x@printlist, map { ($_ => $self->watch_list->{$_}) } @printlist;
	close $watchlist_file;

	return $self;
}

sub grow_stopwords {
	my ( $self, $file ) = @_;
	my ( $watch_factor, $watch_length, $avgfreq );

	$watch_length = scalar keys %{$self->watch_list}; #total number of words in the WATCH_LIST
	$avgfreq = $watch_length ? $self->watch_count / $watch_length : 0; #average frequency of words in WATCH_LIST

	my ($low, $lower, $upper, $high) = ($avgfreq, 0, 0, $avgfreq);
	for my $score (values %{$self->watch_list}) {
		$score > $avgfreq ? $upper += $score : $lower += $score;
		$high = max( $score => $high );
		$low  = min( $score => $low  );
	}

	my $normal    = ($watch_length / 2);  #normalization scalar
	my $whisker   = (3 * $upper - $lower) / (2 * $normal);  #upper whisker
	$watch_factor = $whisker * ($self->watch_coef / log $self->watch_count);  #low-pass threshold

	for (keys %{$self->watch_list}) {
		$self->stopwords->{$_} = 1 if $self->watch_list->{$_} > $watch_factor;
	}


	$upper /=  $normal;
	$lower /=  $normal;
	say "Analyzing file $file";
	say "lower = $lower; mid = $avgfreq; upper = $upper";
	say "upper whisker = $whisker";
	say "avg freq = $avgfreq";
	say "factor = $watch_factor";
	say "\n";
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

	$self->full_text = join "" => map { chomp; $_ } <$file>;

	#module for seperating sentences
	my $splitter = Lingua::Sentence->new("en");
	my $multistring = lc $splitter->split( $self->full_text );

	$multistring =~ s/(\.”|")\s/$1\n/g;
		#manual split — splitter does not work on infix periods (within quotations)
	my @sentences = split /\n/, $multistring;
		#create array of sentences
	my @splitwords = split /\s+|\n+|—|–/, $multistring;
		#create array of every word in order

	$self->multi_str = $multistring;
	$self->sentences = \@sentences;
	$self->word_list = \@splitwords;

	return $self;
}



sub analyze_frequency {
	my $self = shift;


	### FREQUENCY ANALYSES BLOCK
	#
	#creates a hash of words with their frequency of appearance within the article
	my $wordcount = scalar @{$self->word_list};  #counts the total words in the article
	my $min_length = 3;
	my %agg_list;
	for ($self->word_list) {
		if ($_ !~ /\A \W+ \Z/x) {
			s/ [^A-Za-z0-9]+ s? \Z //gx;
			$self->freq_hash->{$_}++ if length $_ >= $min_length and !$self->stopwords->{$_};
		}
	}
	%agg_list = $self->freq_hash;
	my $min_freq = int($wordcount*40/10000) or 1;
	grep { delete $self->freq_hash->{$_} if $self->freq_hash->{$_} < $min_freq } keys %{$self->freq_hash};
		#remove words that appear less than the $min_freq (defaults to 1)
	###
}



sub analyze_clusters {
	my $self = shift;

	### CLUSTER ANALYSES BLOCK
	#
	my $cluster_count;
	for my $index (0..scalar @{$self->sentences} - 1) {
		my @sen_words = split /[^A-Za-z'’\-]+/, $self->sentences->[$index];

		for (0..scalar @sen_words - 1) {
			$cluster_count++;
			for my $f_word (keys %{$self->freq_hash}) {
				if (($sen_words[$_]) =~ /\A$f_word\Z/) {
					my @array;
					push @array, $_ for (@{$self->cluster_hash->{$f_word}}, [$index, $_, $cluster_count]);
					$self->cluster_hash->{$f_word} = \@array;
				}
			}
		}
	}

	my $squaresum;
	my $sum;
	for my $f_word (keys %{$self->cluster_hash}) {
		for my $f_vector (@{$self->cluster_hash->{$f_word}}) {
			$squaresum += $$f_vector[2]**2;
			$sum += $$f_vector[2];
		}
		my $sigma = sqrt( ($squaresum - $sum**2 / $cluster_count) / $cluster_count );	#pop. std. deviation
		$self->sigma_hash->{$f_word} = int( $sigma / 20 );
		$self->sigma_hash->{$_} = $self->sigma_hash->{$_} // 0 for keys %{$self->freq_hash};
	}
	###

	#say Dumper $self->cluster_hash;
	#say Dumper $self->sigma_hash;
}



sub analyze_phrases {
	my $self = shift;

	### PHRASE ANALYSES BLOCK
	#
	my $size = 3;
	my @all_words = split /[^A-Za-z'’\-]+/, $self->multi_str;
	for my $f_word (keys %{$self->cluster_hash}) {
		for my $f_vector (@{$self->cluster_hash->{$f_word}}) {
			my $index = $$f_vector[2] - 1;
			my @array;
			my @phrases = grep { !$self->stopwords->{$_ // 0} } @all_words[ ($index - $size) .. ($index + $size) ];
			push @array, $_ for ( @{$self->phrase_hash->{$f_word}}, \@phrases );
			$self->phrase_hash->{$f_word} = \@array;
		}
	}

	KEYWORD: for my $f_word (keys %{$self->phrase_hash}) {
		OUTER: for ( my $o = 0; $o < scalar @{$self->phrase_hash->{$f_word}}; $o++ ) {
			INNER: for ( my $i = $o + 1; $i < scalar @{$self->phrase_hash->{$f_word}}; $i++ ) {
				my $outer = @{$self->phrase_hash->{$f_word}}[$o];
				my $inner = @{$self->phrase_hash->{$f_word}}[$i];
				grep { $self->inter_hash->{$_}++ if /\w+( \w+)+/ } intersect( $outer, $inner );
					#add phrase if it contains multiple words
			}
		}
	}

	#say Dumper %phrase_hash;
}



### INTERSECT DEFINITION BLOCK
#
sub intersect {
	my ($Llist, $Rlist) = (@_);

	my $Rphrase = join(" ", grep { defined $_ } @$Rlist)
		or return;

	my @intersection;
	my $phrase;
	LWORD: for ( my $o = 0; $o < scalar @$Llist; $o++ ) {
		next LWORD unless $Rphrase =~ qr/$$Llist[$o]/;
		$phrase = $$Llist[$o];
		
		for ( my $i = $o + 1; $i < scalar @$Llist; $i++ ) {
			next LWORD unless $Rphrase =~ qr/$phrase $$Llist[$i]/;
			$phrase .= " ${$Llist}[$i]";
		}
		
		push @intersection, $phrase;
	}

		#say Dumper @intersection;

	return @intersection;
}
###



sub pretty_printer {
	my $self = shift;

	say Dumper $self->inter_hash;

	### PRETTY PRINT BLOCK
	#
	my %inter_sorted;
	for my $phrase (keys %{$self->inter_hash}) {
		$inter_sorted{$_} += $self->inter_hash->{$phrase} for split " ", $phrase;
	}

	my %sort_list;
	for (keys %{$self->freq_hash}) {
		$sort_list{$_} += $self->freq_hash->{$_} // 0;
		$sort_list{$_} += $self->sigma_hash->{$_} // 0;
		$sort_list{$_} += $inter_sorted{$_} // 0;
	}
	printf "%${$self->maxword}s|%s%s%s\n", ( $_,"-"x$self->freq_hash->{$_}, "+"x$self->sigma_hash->{$_}, "-"x($inter_sorted{$_} // 0) )
		for sort { $sort_list{$b} <=> $sort_list{$a} } keys %sort_list;
	say "";
	###
}




1;




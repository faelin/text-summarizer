package Text::Summarizer;

our $VERSION = "0.02";

use v5.10.0;
use strict;
use warnings;
use Lingua::Sentence;
use Moo;
use Types::Standard qw/ Ref Str Int Num InstanceOf /;
use List::AllUtils qw/ max min sum /;
use Data::Dumper::Sorted qw/ Dumper /;
use utf8;
binmode STDOUT, ":utf8";

use Benchmark ':hireswallclock';


has permanent_path => (
	is  => 'ro',
	isa => Str,
	default => 'data/permanent.stop',
);

has stopwords_path => (
	is  => 'ro',
	isa => Str,
	default => 'data/stopwords.stop',
);

has watchlist_path => (
	is  => 'ro',
	isa => Str,
	default => 'data/watchlist.stop'
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

has phrase_list => (
	is => 'rwp',
	isa => Ref['HASH'],
);

has phrase_radius => (
	is => 'rwp',
	isa => Int,
	default => 5,
);

has phrase_threshold => (
	is => 'rwp',
	isa => Int,
	default => 2,
);

has phrase_min => (
	is => 'rwp',
	isa => Int,
	default => 2,
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

has sentences => (
	is => 'rwp',
	isa => Ref['ARRAY'],
);

has word_list => (
	is => 'rwp',
	isa => Ref['ARRAY'],
);

has sen_words => (
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

has score_hash => (
	is => 'rwp',
	isa => Ref['HASH'],
);

has max_word_length => (
	is => 'rwp',
	isa => Int,
	default => 0,
);

has word_length_threshold => (
	is => 'rwp',
	isa => Int,
	default => 3,
);

has article_length => (
	is => 'rwp',
	isa => Int,
	default => 0,
	lazy => 1,
);

has max_score => (
	is => 'rwp',
	isa => Int,
	default => 0,
);

has freq_constant => (
	is => 'rwp',
	isa => Num,
	default => 0.004,
);

has return_count => (
	is => 'rwp',
	isa => Num,
	default => 20,
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
	$self->_set_max_word_length( max map { length } keys %watch_list // 0 );  #generates the length of the longest word collected
	$self->_set_max_score( max map { length } values %watch_list // 0 );  #generates the length of the longest score collected

	return \%watch_list;
}

sub load_stopwords {
	my $self = shift;
	my %stopwords;

	open( my $permanent_file, '<', $self->permanent_path )
 		or die "Can't open " . $self->permanent_path . ": $!";
	open( my $stopwords_file, '<', $self->stopwords_path )
		or die "Can't open " . $self->stopwords_path . ": $!";

	chomp and $stopwords{ $_ } = 1 for (<$permanent_file>);
	chomp and $stopwords{ $_ } = 1 for (<$stopwords_file>);

	close $permanent_file;
	close $stopwords_file;

	return \%stopwords;
}



#scanning is used to augment the *watchlist* and *stopwords* attributes
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



#summarizing is used to extract common phrase fragments from a given text file.
sub summarize_file {
	my ($self, $filepath) = @_;

	open( my $file, '<:utf8', $filepath )
		or die "Can't open $filepath: $!";

	say "\nSummary of file $filepath";

	$self->watchlist;
	$self->stopwords;

			my $t1 = Benchmark->new;

	$self->tokenize( $file );	#breaks the provided file into sentences and individual words
			my $t2 = Benchmark->new;

	$self->analyze_phrases;		#
			my $t3 = Benchmark->new;


			my $td1 = timediff($t2, $t1);
			my $td2 = timediff($t3, $t2);
			say "\t  SPLIT: ",timestr($td1);
			say "\tANALYZE: ",timestr($td2);

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
	$self->_set_max_word_length( max map { length } keys %{$self->watchlist} );  #generates the length of the longest word collected
	$self->_set_max_score( max map { length } values %{$self->watchlist} );  #generates the length of the longest score collected

	return $self;
}

sub store_watchlist {
	my $self = shift;

	my @printlist = sort { $self->watchlist->{$b} <=> $self->watchlist->{$a} } keys %{$self->watchlist};

	open( my $watchlist_file, ">", $self->watchlist_path )
		or die "Can't open $self->watchlist_path: $!";

	my $string = "\%" . $self->max_word_length . "s | \%" . $self->max_score . "s\n";
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



sub tokenize {
	my ( $self, $file ) = @_;

	my $full_text = join "\n" => map { $_ } <$file>;
		#contains the full body of text
	my @sentences = split qr/(?|   (?<=(?<!\s[djms]r) (?<!\s[djms]rs) \.  |  \!  |  \?)  \s+\n?
							   |   \s{3,}
							   |   \s*\n\s*
							   |   (?: (?<![A-Za-z0-9-]) > \s+)+
							   |   (?: ^\s+$ )
							   |   (?: ^$)
							 )/mix => $full_text;
		# array of sentences

	my @word_list;  # array literal of all the words in the entire text body
	my @sen_words; # array reference to all of the tokens in each sentence
	for (@sentences) {  #creates an array of each word in the current article that is not a stopword and is longer than the given *word_length_threshold*
		my @words = map { /\b (?: \w \. (?: ['’-] \w+ )?)+ | (?: \w+ ['’-]? )+ (?=\s|\b)/gx } lc $_;  #tokenizes each sentence into complete words (single-quotes are considered part of the word they attach to)
		push @word_list =>  @words;
		push @sen_words => \@words;
	}

	$self->_set_article_length( scalar @word_list ); 
		#counts the total number of words in the article

	$self->_set_full_text( $full_text  );
	$self->_set_sentences( \@sentences );
	$self->_set_word_list( \@word_list );
	$self->_set_sen_words( \@sen_words );


	return $self;
}



sub analyze_phrases {
	my $self = shift;


	my $min_freq_thresh = int($self->article_length * $self->freq_constant) // 1; #estimates a minimum threshold of occurence for frequently occuring words
	my %freq_hash; #counts the number of times each word appears in the *%word_list* hash
	for my $word (@{$self->word_list}) {
	 	$freq_hash{$word}++ unless $self->stopwords->{$word};
	}
	grep { delete $freq_hash{$_} if $freq_hash{$_} < $min_freq_thresh } keys %freq_hash;
		#remove words that appear less than the *$min_freq_thresh* (defaults to 1)
	$self->_set_freq_hash( \%freq_hash );


	my (%cluster_hash, %cluster_count);
	for my $sen_index (0..scalar @{$self->sentences} - 1) { #gives the index of each sentence in the article
		my @sen_words = @{$self->sen_words->[$sen_index]}; 
						# creates an array of each word in the given sentence,
						# given that the word is not a stopword and is longer than the given *word_length_threshold*
		
		for my $position (0..scalar @sen_words - 1) { #iterates across each word in the sentence
			if ( exists $self->freq_hash->{$sen_words[$position]}) { ## true if the given word at index *position* appears in the *freq_hash*
				my %word = ( sen => $sen_index, pos => $position, cnt => ++$cluster_count{$sen_words[$position]} );
					# hash-vector of three elements:
					#	sen => the index of the current sentence
					#	pos => index of the current word within the current sentence
					#	cnt => number of times the given word has appeared in the entire text file
				push @{$cluster_hash{$sen_words[$position]}} => \%word;
			}
		}
	}
	$self->_set_cluster_hash( \%cluster_hash );


	#create long-form phrases around frequently used words by tracking forward and backward *phrase_radius* from any given *c_word*
	my (%phrase_hash, %sigma_hash);
	for my $c_word (keys %{$self->cluster_hash}) {
		for my $c_vector (@{$self->cluster_hash->{$c_word}}) {

			my ($sen, $pos, $cnt) = @$c_vector{'sen', 'pos', 'cnt'};
			# *sen* indicates which sentence the current *c_word* appears in
			# *pos* indicates the position of the *c_word* within the sentence (see above)
			# *cnt* counts the total number of times the word has been detected thus far

			my @phrase = @{$self->sen_words->[$sen]}[ max($pos - $self->phrase_radius, 0) .. min($pos + $self->phrase_radius, scalar(@{$self->sen_words->[$sen]}) - 1) ];
				#array slice containing only tokens within *phrase_radius* of the *c_word* within the given sentence

			unshift @phrase => $self->sentences->[$sen]; #begins the *phrase* array with a complete, unedited sentence (for reference only)
			push @{$phrase_hash{$c_word}} => \@phrase if scalar @phrase > $self->phrase_threshold + 1;
				#the *phrase_hash* can only contain a given *phrase* array if it is longer than the defined *phrase_threshold* + 1  (defaults to 3)
		}

		#the following is used for scoring purposes, and is used only to determine the *sigma* score (population standard deviation) of the given *c_word*
		my $cluster_count = $self->cluster_hash->{$c_word}->[-1]->{cnt};
		$sigma_hash{$c_word} = int sqrt( ($cluster_count**2 - ($cluster_count**2 / $cluster_count)) / $cluster_count );	#pop. std. deviation
	}

	$self->_set_sigma_hash( \%sigma_hash );
	$self->_set_phrase_hash( \%phrase_hash );


	#find common phrase-fragments
	my (%inter_hash, %score_hash, %full_phrases); #*inter_hash* contains phrase fragments;  *score_hash* contains score values for words in those phrases
	F_WORD: for my $f_word (keys %{$self->phrase_hash}) {


		my (@hash_list, %sum_list); #*hash_list* contains ordered, formatted lists of each word in the phrase fragment;  *sum_list* contains the total number of times each word appears in all phrases for the given *f_word*
		ORDER: for my $phrase (@{$self->phrase_hash->{$f_word}}) {
			my %ordered_words = map { $sum_list{$phrase->[$_]}++; ($_ => $phrase->[$_]) } (1..scalar @{$phrase} - 1);
				# *words* contains an ordered, formatted list of each word in the given phrase fragment, looks like:
				# 	'01' => 'some'
				#	'02' => 'word'
				#	'03' => 'goes'
				# 	'04' => 'here'
			my %full_phrase = %ordered_words;
			push @hash_list => [$f_word, \%full_phrase, \%ordered_words];
		}


		#removes each word from the *word_hash* unless it occurs more than once amongst all phrases
		SCRAP: for my $word_hash (@hash_list) {
			for my $word (keys %{$word_hash->[-1]}) {
				delete $word_hash->[-1]->{$word} unless $sum_list{$word_hash->[-1]->{$word}} > 1
			}
		}


		#break phrases fragments into "scraps" (consecutive runs of words within the fragment)
		my @frag_list;
		 FRAG: for my $word_hash (@hash_list) {
			my (%L_scrap, %R_scrap); #a "scrap" is a sub-fragment
			my ($prev, $curr, $next) = (-1,0,0); #used to find consecutive sequences of words
			my $real = 0; #flag for stopwords identification

			my @word_keys = sort { $a <=> $b } keys %{$word_hash->[-1]}; # *word_keys* contains a series of index-values
			for (my $i = 0; $i < scalar @word_keys; $i++ ) {
				$curr = $word_keys[$i];
				$next = $word_keys[$i+1] if $i < scalar @word_keys - 1; # if-statement prevents out-of-bounds error

				if ( $next == $curr + 1 or $curr == $prev + 1 ) {
					unless ($curr == $prev + 1) {  #resets *R_scrap* when the *curr* index skips over a number (i.e. a new scrap is encountered)
						%L_scrap = %R_scrap if keys %L_scrap <= keys %R_scrap; #chooses the longest or most recent scrap
						%R_scrap = (); #resets the *R_scrap*
					}
					$R_scrap{$curr} = $word_hash->[-1]->{$curr};
					$real = 1 unless $self->stopwords->{$R_scrap{$curr}}; #ensures that scraps consisting only of stopwords are ignored
				} else {
					%L_scrap = %R_scrap if keys %L_scrap <= keys %R_scrap; #chooses the longest or most recent scrap
					%R_scrap = (); #resets the *R_scrap*
				}
				$prev = $curr;
			}
			%L_scrap = %R_scrap if keys %L_scrap <= keys %R_scrap; #chooses the longest or most recent scrap
			%R_scrap = (); #resets the *R_scrap*
			push @frag_list => [$word_hash->[0], $word_hash->[1], $word_hash->[2], \%L_scrap] if $real and scalar keys %L_scrap >= $self->phrase_threshold;
		}


		#compile scraps for scoring
		 JOIN: for my $fragment (@frag_list) {
			my $scrap  = join ' ' => map { $score_hash{$_}++;
										   $fragment->[-1]->{$_} } sort { $a <=> $b } keys %{$fragment->[-1]};
			my $phrase = join ' ' => map { $fragment->[ 1]->{$_} } sort { $a <=> $b } keys %{$fragment->[ 1]};

			$score_hash{$f_word}++;
			$inter_hash{$scrap}++;
			$full_phrases{$phrase}++;
		}
	}


	#each phrases' score is multiplied by the sum of the compound score of each word within the phrase
	for my $scrap (keys %inter_hash) {
		for my $word (split ' ' => $scrap) {
			my $score = 1;
			$score += $freq_hash{$word}  // 0;
			$score += $sigma_hash{$word} // 0;
			$score += $score_hash{$word} // 0;

			$inter_hash{$scrap} *= $score;
		}
	}


	#combine scraps — if scrap "a" contains scrap "b", add the value of "b" to "a" and delete "b"
	CLEAR: for my $scrap (sort { $inter_hash{$b} <=> $inter_hash{$a} or $a cmp $b } keys %inter_hash) {
		my $compare = qr/\b$scrap\b/;
		my $delete  = 0;
		for my $test (keys %inter_hash) {
			if ($test ne $scrap and $test =~ /$compare/) {
				$inter_hash{$test} += $inter_hash{$scrap};
				$delete = 1;
			}
		}
		delete $inter_hash{$scrap} if $delete;
	}


	$self->_set_score_hash(  \%score_hash );
	$self->_set_inter_hash(  \%inter_hash );
	$self->_set_phrase_list( \%full_phrases );


	return $self;
}



sub pretty_printer {
	my $self = shift;

	say "PHRASES:";

	my @phrase_keys = sort { $self->inter_hash->{$b} <=> $self->inter_hash->{$a} or $a cmp $b } keys %{$self->inter_hash};
	for my $phrase (@phrase_keys[0..min($self->return_count,scalar @phrase_keys - 1)]) {
		say "\t$phrase => " . $self->inter_hash->{$phrase};
	} 
	say "\n";

	my %sort_list;
	for (keys %{$self->freq_hash}) {
		$sort_list{$_} += $self->freq_hash->{$_}  // 0;
		$sort_list{$_} += $self->sigma_hash->{$_} // 0;
		$sort_list{$_} += $self->score_hash->{$_} // 0;
	}

	say "WORDS:";

	my @sort_list_keys = sort { $sort_list{$b} <=> $sort_list{$a} or $a cmp $b } keys %sort_list;
	my $highest = $sort_list{$sort_list_keys[0]};
		#my $average = sum(values %sort_list) / scalar @sort_list_keys;
	my $longest = max map {length} @sort_list_keys;
	KEY: for ( @sort_list_keys[0..min($self->return_count,scalar @sort_list_keys - 1)] ) {
		next KEY if $self->stopwords->{$_};
		my $format = "%" . $longest . "s|%s\n";
		my $score = int(40*$sort_list{$_}/$highest);
		printf $format => ( $_ , "-" x $score ) if $score > 2;
	}
	say "";

	return $self;
}




1;




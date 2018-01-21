package Text::Summarizer;

use v5.10.0;
use strict;
use warnings;
use Moo;
use Types::Standard qw/ Ref Str Int Num InstanceOf /;
use List::AllUtils qw/ max min sum singleton /;
use utf8;

use vars qw(@ISA @EXPORT @EXPORT_OK %EXPORT_TAGS $VERSION);
require Exporter;

@ISA = qw(Exporter);
@EXPORT = qw();
@EXPORT_OK = qw();
%EXPORT_TAGS = (all => [@EXPORT_OK]);
$VERSION = '1.0206';


has permanent_path => (
	is  => 'rw',
	isa => Str,
	default => 'data/permanent.stop',
);

has stopwords_path => (
	is  => 'rw',
	isa => Str,
	default => 'data/stopwords.stop',
);

has watchlist_path => (
	is  => 'rw',
	isa => Str,
	default => 'data/watchlist.stop'
);

has articles_path => (
	is => 'rw',
	isa => Str,
	default => 'articles/*'
);

has freq_constant => (
	is => 'ro',
	isa => Num,
	default => 0.004,
);

has word_length_threshold => (
	is => 'ro',
	isa => Int,
	default => 3,
);

has phrase_radius => (
	is => 'ro',
	isa => Int,
	default => 5,
);

has phrase_threshold => (
	is => 'ro',
	isa => Int,
	default => 2,
);

has phrase_min => (
	is => 'ro',
	isa => Int,
	default => 2,
);

has watch_coef => (
	is => 'ro',
	isa => Int,
	default => 30,
);

has watch_count => (
	is => 'rwp',
	isa => Int,
	default => 0,
);

has watchlist => (
	is => 'lazy',
	isa => Ref['HASH'],
	builder => 'load_watchlist',
);

has stopwords => (
	is => 'lazy',
	isa => Ref['HASH'],
	builder => 'load_stopwords',
);

has article_length => (
	is => 'rwp',
	isa => Int,
	default => 0,
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

has phrase_list => (
	is => 'rwp',
	isa => Ref['HASH'],
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

	$self->_set_watch_count( sum values %{$self->watchlist} // 0 );  #counts the total number of watch_words ever collected

	return \%watch_list;
}

sub load_stopwords {
	my $self = shift;
	my %stopwords;

	open( my $permanent_file, '<', $self->permanent_path )
 		or die "Can't open " . $self->permanent_path . ": $!";
	chomp and $stopwords{ $_ } = 1 for (<$permanent_file>);
	close $permanent_file;

	open( my $stopwords_file, '<', $self->stopwords_path )
		or die "Can't open " . $self->stopwords_path . ": $!";
	chomp and $stopwords{ $_ } = 1 for (<$stopwords_file>);
	close $stopwords_file;

	return \%stopwords;
}



#scanning is used to augment the *watchlist* and *stopwords* attributes
sub scan_file {
	my ($self, $file_path) = @_;

	open( my $file, "<", $file_path )
		or die "Can't open $file_path: $!";

	say "\nAnalyzing file $file_path";
	$self->grow_watchlist( $file );
	$self->grow_stopwords( $file );
	$self->store_watchlist;
	$self->store_stoplist;

	return $self;

}

sub scan_all {
	my ($self, $dir_path) = @_;

	$self->scan_file( $_ ) foreach glob($dir_path // $self->articles_path);

	return $self;
}

sub grow_watchlist {
	my ($self, $file) = @_;

	for (<$file>) {
		for my $word ( map { /\b (?: \w \. (?: ['’-] \w+ )?)+ | (?: \w+ ['’-]? )+ (?=\s|\b)/gx } lc $_ ) {
			$self->watchlist->{$word}++ unless ( exists $self->stopwords->{$word} );
		}
	}

	$self->_set_watch_count( sum values %{$self->watchlist} // 0 );  #counts the total number of watch_words ever collected

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

sub store_watchlist {
	my $self = shift;

	my @printlist = sort { $self->watchlist->{$b} <=> $self->watchlist->{$a} } keys %{$self->watchlist};

	my $max_word_length = max map { length } keys %{$self->watchlist};  #generates the length of the longest word collected
	my $max_score_length = max map { length } values %{$self->watchlist};  #generates the length of the longest score collected

	my $format = "\%" . $max_word_length . "s | \%" . $max_score_length . "s\n";

	open( my $watchlist_file, ">", $self->watchlist_path )
		or die "Can't open $self->watchlist_path: $!";
	printf $watchlist_file $format x @printlist, map { ($_ => $self->watchlist->{$_}) } @printlist;
	close $watchlist_file;

	return $self;
}

sub store_stoplist {
	my $self = shift;

	open( my $stopwords_file, ">", $self->stopwords_path)
		or die "Can't open $self->stopwords_file: $!";
	print $stopwords_file "$_\n" for sort keys %{$self->stopwords};
	close $stopwords_file;

	return $self;
}



sub summarize_text {
	my ($self, $text) = @_;

	$self->tokenize( $text );	#breaks the provided file into sentences and individual words
	$self->analyze_phrases;		#analyzes the frequency and clustering of words within the provided file

	return $self->summary;
}

#summarizing is used to extract common phrase fragments from a given text file.
sub summarize_file {
	my ($self, $file_path) = @_;

	open( my $file, '<:utf8', $file_path )
		or die "Can't open $file_path: $!";

	my $text = join "\n" => map { $_ } <$file>;

	return $self->summarize_text($text);
}

sub summarize_all {
	my ($self, $dir_path) = @_;
	return map { $self->summarize_file( $_ ) } glob($dir_path // $self->articles_path);
}



sub tokenize {
	my ( $self, $text ) = @_;

	my $full_text = $text;
		#contains the full body of text
	my @sentences = split qr/(?|   (?<=(?<!\s[djms]r) (?<!\s[djms]rs) \.  |  \!  |  \?)  (?: (?=[A-Z])|\s+)
							   |   (?: (?<![A-Za-z0-9-]) > \s+)+
							   |   (?: ^\s+$ )
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
	my (%inter_hash, %score_hash, %bare_phrase, %full_phrase); #*inter_hash* contains phrase fragments;  *score_hash* contains score values for words in those phrases
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
			my @bare   = map { $fragment->[-1]->{$_} } grep { !$self->stopwords->{$fragment->[-1]->{$_}} } sort { $a <=> $b } keys %{$fragment->[-1]};

			$score_hash{$f_word}++;  #scores each *f_word*
			$inter_hash{$scrap}++;   #contains the final *L_scrap*


			my $score = 1;
			for my $word (split ' ' => $scrap) {
				$score += $freq_hash{$word}  // 0;
				$score += $sigma_hash{$word} // 0;
				$score += $score_hash{$word} // 0;
			}

			$full_phrase{$self->phrase_hash->{$f_word}->[0]->[0]} += $score; #contains the full phrase from which the *L_scrap* was drawn
			$bare_phrase{$scrap} = \@bare if scalar @bare;   #contains the final *L_scrap* without any stopwords
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
		TEST: for my $test (keys %inter_hash) {
			if ($test ne $scrap) {
				if ($test =~ /$compare/) { #true iff  *scrap* ∈ *test*
					$inter_hash{$test} += $inter_hash{$scrap};
					delete $inter_hash{$scrap} and next CLEAR;
				} elsif (not scalar singleton (@{$bare_phrase{$test}}, @{$bare_phrase{$scrap}}) ) { #true iff *bare_phrase{test}* == *bare_phrase{scrap}*
					next TEST unless scalar @{$bare_phrase{$test}} > 1;

					my $joined = join '|' => @{$bare_phrase{$test}};
					$inter_hash{"($joined)"} = $inter_hash{$test} + $inter_hash{$scrap};
					$inter_hash{$test} += $inter_hash{$scrap};
					delete $inter_hash{$scrap} and next CLEAR;
				}
			}
		}
	}


	$self->_set_score_hash(  \%score_hash );
	$self->_set_inter_hash(  \%inter_hash );
	$self->_set_phrase_list( \%full_phrase );


	return $self;
}



#returns a summary array for the given text, in the form of a hash of array-refs:
#	sentences => a list of full sentences from the given article, scored based on the scores of the words contained therein
#	fragments => a list of phrase fragments from the given article, scored as above
#	    words => a list of all words in the article, scored by a three-factor system consisting of
#				(frequency of appearance, population standard deviation, and use in important phrase fragments)
sub summary {
	my $self = shift;

	my %sort_list;
	for (keys %{$self->freq_hash}) {
		$sort_list{$_} += $self->freq_hash->{$_}  // 0;
		$sort_list{$_} += $self->sigma_hash->{$_} // 0;
		$sort_list{$_} += $self->score_hash->{$_} // 0;
	}

	my %sentences = map { ($_ => $self->phrase_list->{$_}) } sort { $self->phrase_list->{$b} <=> $self->phrase_list->{$a} } keys %{$self->phrase_list};
	my %fragments = map { ($_ => $self->inter_hash->{$_})  } sort { $self->inter_hash->{$b} <=> $self->inter_hash->{$a} or $a cmp $b } keys %{$self->inter_hash};
	my %singleton = map { ($_ => $sort_list{$_}) 		   } sort { $sort_list{$b} <=> $sort_list{$a} or $a cmp $b } keys %sort_list;

	return { sentences => \%sentences, fragments => \%fragments, words => \%singleton };
}



sub pretty_print {
	my ($self, $summary, $return_count) = @_;
	my ($sentences, $fragments, $words) = @{$summary}{'sentences','fragments','words'};

	binmode STDOUT, ":utf8";

	say "SUMMARY:";
	my @sentence_keys = sort { $sentences->{$b} <=> $sentences->{$a} or $a cmp $b} keys %$sentences;
	for my $sen ( @sentence_keys[0..min($return_count,scalar @sentence_keys - 1)] ) {
		printf "%4d => %100s\n" => $sentences->{$sen}, $sen;
	}
	say "\n";


	say "PHRASES:";
	my @phrase_keys = sort { $fragments->{$b} <=> $fragments->{$a} or $a cmp $b } keys %$fragments;
	for my $phrase ( @phrase_keys[0..min($return_count,scalar @phrase_keys - 1)] ) {
		printf "%8d => %s\n" => $fragments->{$phrase}, $phrase;
	} 
	say "\n";


	say "  WORDS:";
	my @word_keys = sort { $words->{$b} <=> $words->{$a} or $a cmp $b } keys %$words;
	my $highest = $words->{$word_keys[0]};
	my $longest = max map {length} @word_keys;
	KEY: for my $word ( @word_keys[0..min($return_count,scalar @word_keys - 1)] ) {
		my $format = "%" . $longest . "s|%s\n";
		my $score = int(40*$words->{$word}/$highest);
		printf $format => ( $word , "-" x $score ) if $score > 2;
	}
	say "\n";


	return $summary;
}



1;
__END__

	

=pod
 
=encoding utf-8

=head1 NAME

Text::Summarizer - Summarize Bodies of Text

=head1 SYNOPSIS

	use Text::Summarizer;
	
	my $summarizer = Text::Summarizer->new( articles_path => "articles/*" );
	
	my $summary   = $summarizer->summarize_file("articles/article00.txt");
		#or if you want to process in bulk
	my @summaries = $summarizer->summarize_all("articles/*");
	
	$summarizer->pretty_print($summary);
	$summarizer->pretty_print($_) for (@summaries);

=head1 DESCRIPTION

This module allows you to summarize bodies of text into a scored hash of I<sentences>, I<phrase-fragments>, and I<individual words> from the provided text.
These scores reflect the weight (or precedence) of the relative text-fragments, i.e. how well they summarize or reflect the overall nature of the text.
All of the sentences and phrase-fragments are drawn from within the existing text, and are NOT proceedurally generated.

C<< $summarizer->summarize_text >> and C<< $summarizer->summarize_file >> each return a hash-ref containing three array-refs (C<< $summarizer->summarize_all >> returns a list of these hash-refs):

=over 2

=item B<sentences>
a list of full sentences from the given article, with composite scores of the words contained therein

=item B<fragments>
a list of phrase fragments from the given article, scored as above

=item B<    words>
a list of all words in the article, scored by a three-factor system consisting of I<frequency of appearance>, I<population standard deviation of word clustering>, and I<use in selected phrase fragments>.

The C<< $summarizer->pretty_print >> method prints a visually pleasing graph of the above three summary categories.

=back

=head2 About Fragments

Phrase fragments are in actuallity short "scraps" of text (usually only two or three words) that are derived from the text via the following process:

=over 8

=item 1

the entirety of the text is tokenized and scored into a C<frequency> table, with a high-pass threshold of frequencies above C<# of tokens * user-defined scaling factor>

=item 2

each sentence is tokenized and stored in an array

=item 3

for each word within the C<frequency> table, a table of phrase-fragments is derived by finding each occurance of said word and tracking forward and backward by a user-defined "radius" of tokens (defaults to C<radius = 5>, does not include the central key-word) — each phrase-fragment is thus compiled of (by default) an 11-token string

=item 4

all fragments for a given key-word are then compared to each other, and each word is deleted if it appears only once amongst all of the fragments
(leaving only C<I<A> ∪ I<B> ∪ ... ∪ I<S>> where I<A>, I<B>,...,I<S> are the phrase-fragments)

=item 5

what remains of each fragment is a list of "scraps" — strings of consecutive tokens — from which the longest scrap is chosen as a representation of the given phrase-fragment

=item 6

when a shorter fragment-scrap is included in the text of a longer scrap (i.e. a different phrase-fragment), the shorter is deleted and its score is added to the score of the longer

=item 7

when multiple fragments are equivalent (i.e. they consist of the same list of tokens when stopwords are excluded), they are condensed into a single scrap in the form of C<"(some|word|tokens)"> such that the fragment now represents the tokens of the scrap (excluding stopwords) regardless of order

=back

=head1 AUTHOR

Faelin Landy (CPAN: FAELIN) L<faelin.landy@gmail.com>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2018 by Faelin Landy

This program is free software: you can redistribute it and/or modify it under the terms of the GNU Lesser General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.


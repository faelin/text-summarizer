#!/usr/bin/perl

package Text::Summarize;

use v5.10;
use Data::Dumper::Simple;
use List::Compare;
use Lingua::Sentence;
use strict;
use warnings;
use utf8;

use Benchmark ':hireswallclock';
my $t0 = Benchmark->new;


### OPEN FILE BLOCK
#
#initiate stopwords
my $stopwords_path = "words/stopwords.log";
open( my $stopwords_file, "<", $stopwords_path )
 	or die "Can't open $stopwords_path: $!";


my $permanent_path = "words/permanent.txt";
open( my $permanent_file, "<", $permanent_path )
 	or die "Can't open $permanent_path: $!";

my $watchlist_path = "words/watchlist.log";
open( my $watch_file, "<", $watchlist_path )
	or die "Can't open $watchlist_path: $!";

if ( $ARGV[0] ) {
}
###


### LOAD WATCH_LIST BLOCK
#
my %watch_list;
my ($watch_count, $maxword, $maxnum) = (0,1,1);
for (<$watch_file>) {
	if (/\s*(\w+) \| \s*(\d+)/) {
		$watch_list{$1} = $2;
		$watch_count += $2;
		$maxword = length $1 if $maxword < length $1;
		$maxnum = length $2 if $maxnum < length $2;
			#generates the length of the longest word/number collected
	}
}
close $watch_file;
###


### LOAD COMMON WORDS BLOCK
#
#creates a hash of many common words with their ranking (1 = most common)
my %stopwords;
for (<$permanent_file>) {
	chomp;
	$stopwords{ $_ } = 1;
}
close $permanent_file;
###


### MAIN
#
foreach my $filepath ( <articles/*.txt> ) {
	open( my $file, "<", $filepath )
		or die "Can't open $filepath: $!";

	for (<$file>) {
		while ( m/ ([A-Za-z]+ (?:['’][A-Za-z]{2,}|[[A-Za-z]-[s]])? ) /xg ) {
			my $word = lc $1;
			unless ( exists $stopwords{$word} ) {
				$watch_list{$word}++;
				$watch_count++;
				$maxword = length $word if $maxword < length $word;	#generates the length of the longest word collected
				$maxnum = length $watch_list{$word} if $maxnum < length $watch_list{$word};
			}
		}
	}
}
###


### WATCHLIST POPULATION BLOCK
#
my @printlist = sort { $watch_list{$b} <=> $watch_list{$a} } keys %watch_list;
open( $watch_file, ">", $watchlist_path )
	or die "Can't open $watchlist_path: $!";
printf $watch_file "\%${maxword}s | \%${maxnum}s\n"x@printlist, map { ($_ => $watch_list{$_}) } @printlist;
close $watch_file;
###


### STOPWORDS POPULATION BLOCK
#
#uses the average frequency of words to determine the max frequency for words added to the f_list;
for (<$stopwords_file>) {
	chomp;
	$stopwords{ $_ } = 1;
}
close $stopwords_file;


my $watch_coef = 30; #arbitrary coefficient to determine the low-pass ceiling
my $watch_length = scalar keys %watch_list; #total number of words in the WATCH_LIST
my $avgfreq = $watch_length ? $watch_count / $watch_length : 0; #average frequency of words in WATCH_LIST


my ($low, $lower, $upper, $high) = ($avgfreq, 0, 0, $avgfreq);
for (values %watch_list) {
	$_ > $avgfreq ? $upper += $_ : $lower += $_;
	$high = $_ if $_ > $high;
	$low = $_ if $_ < $low;
}

$upper = $upper / ($watch_length / 2);
$lower = $lower / ($watch_length / 2);


say "lower = $lower; mid = $avgfreq; upper = $upper";
say "upper whisker = " . ($upper + ($upper - $lower) / 2);


#my $watch_factor = $avgfreq ? $avgfreq + $watch_coef / log $watch_count : 10000;
my $watch_factor = ($upper + ($upper - $lower) / 2) * ($watch_coef / log $watch_count);
for (keys %watch_list) {
	$stopwords{$_} = 1 if $watch_list{$_} > $watch_factor;
}
open( $stopwords_file, ">", $stopwords_path)
	or die "Can't open $stopwords_file: $!";
print $stopwords_file "$_\n" for sort keys %stopwords;
close $stopwords_file;
###

say "avg freq = $avgfreq";
say "factor = $watch_factor";
say "\n";


if ( $ARGV[0] ) {
	### SPLIT SENTENCES BLOCK
	#
	#module for seperating sentences
	my $article_path = "articles/$ARGV[0]";
	open( my $article_file, "<", $article_path )
		or die "Can't open $article_path: $!";

	my $splitter = Lingua::Sentence->new("en");
	my $multistring = lc $splitter->split( join( " ", map { chomp; $_ } <$article_file> ) );

	$multistring =~ s/(\.”|")\s/$1\n/g;
		#manual split — splitter does not work on infix periods (within quotations)
	my @sentences = split /\n/, $multistring;
		#create array of sentences
	my @splitwords = split /\s+|\n+|—|–/, $multistring;
		#create array of every word in order
	my $wordcount = scalar @splitwords;
		#counts the total words in the article
	###


	### FREQUENCY ANALYSES BLOCK
	#
	#creates a hash of words with their frequency of appearance within the article
	my $min_length = 3;
	my %f_list;
	my %agg_list;
	for (@splitwords) {
		if ($_ !~ /\A \W+ \Z/x) {
			s/ [^A-Za-z0-9]+ s? \Z //gx;
			$f_list{$_}++ if length $_ >= $min_length and !$stopwords{$_};
		}
	}
	%agg_list = %f_list;
	my $min_freq = int($wordcount*40/10000) or 1;
	grep { delete $f_list{$_} if $f_list{$_} < $min_freq } keys %f_list;
		#remove words that appear less than the $min_freq (defaults to 1)
	###be


	#say Dumper %f_list;


	### CLUSTER ANALYSES BLOCK
	#
	my %cluster_list;
	my $cluster_count;
	for my $index (0..scalar @sentences - 1) {
		my @sen_words = split /[^A-Za-z'’\-]+/, $sentences[$index];

		for (0..scalar @sen_words - 1) {
			$cluster_count++;
			for my $f_word (keys %f_list) {
				if (($sen_words[$_]) =~ /\A$f_word\Z/) {
					my @array;
					push @array, $_ for (@{$cluster_list{$f_word}}, [$index, $_, $cluster_count]);
					$cluster_list{$f_word} = \@array;
				}
			}
		}
	}

	my $squaresum;
	my $sum;
	my %sigma_list;
	for my $f_word (keys %cluster_list) {
		for my $f_vector (@{$cluster_list{$f_word}}) {
			$squaresum += $$f_vector[2]**2;
			$sum += $$f_vector[2];
		}
		my $sigma = sqrt( ($squaresum - $sum**2 / $cluster_count) / $cluster_count );	#pop. std. deviation
		$sigma_list{$f_word} = int( $sigma / 20 );
		$sigma_list{$_} = $sigma_list{$_} // 0 for keys %f_list;
	}
	###

	#say Dumper %cluster_list;
	#say Dumper %sigma_list;


	my $t1 = Benchmark->new;


	### PHRASE ANALYSES BLOCK
	#
	my $size = 3;
	my %phrase_list;
	my @all_words = split /[^A-Za-z'’\-]+/, $multistring;
	for my $f_word (keys %cluster_list) {
		for my $f_vector (@{$cluster_list{$f_word}}) {
			my $index = $$f_vector[2] - 1;
			my @array;
			my @phrases = grep { !$stopwords{$_ // 0} } @all_words[ ($index - $size) .. ($index + $size) ];
			push @array, $_ for ( @{$phrase_list{$f_word}}, \@phrases );
			$phrase_list{$f_word} = \@array;
		}
	}

	my %inter_list;
	for my $f_word (keys %phrase_list) {
		for ( my $o = 0; $o < scalar @{$phrase_list{$f_word}}; $o++ ) {
			for ( my $i = $o + 1; $i < scalar @{$phrase_list{$f_word}}; $i++ ) {
				my $outer = @{$phrase_list{$f_word}}[$o];
				my $inner = @{$phrase_list{$f_word}}[$i];
				grep { $inter_list{$_}++ if /\w+( \w+)+/ } intersect( $outer, $inner );
					#add phrase if it contains multiple words
			}
		}
	}
	###


	#say Dumper %phrase_list;


	### INTERSECT DEFINITION BLOCK
	#
	sub intersect {
		my ($Llist, $Rlist) = (@_);

		my $Rphrase = join(" ", grep { defined $_ } @$Rlist)
			or return;

		my @intersec;
		my $phrase;
		for ( my $o = 0; $o < scalar @$Llist; $o++ ) {
			next unless $Rphrase =~ /$$Llist[$o]/;
			$phrase = $$Llist[$o];
			
			while ( $o+1 < scalar @$Llist and $Rphrase =~ /$phrase/ ) {
				last unless $Rphrase =~ /$phrase $$Llist[$o+1]/;
				$phrase .= " ${$Llist}[$o+1]";
			}
			
			push @intersec, $phrase;
		}

			#say Dumper @intersec;

		return @intersec;
	}
	###


	my $t2 = Benchmark->new; 


	say Dumper %inter_list;


	### PRETTY PRINT BLOCK
	#
	my %inter_sorted;
	for my $phrase (keys %inter_list) {
		$inter_sorted{$_} += $inter_list{$phrase} for split " ", $phrase;
	}

	my %sort_list;
	for (keys %f_list) {
		$sort_list{$_} += $f_list{$_} // 0;
		$sort_list{$_} += $sigma_list{$_} // 0;
		$sort_list{$_} += $inter_sorted{$_} // 0;
	}
	printf "%${maxword}s|%s%s%s\n", ( $_,"-"x$f_list{$_}, "+"x$sigma_list{$_}, "-"x($inter_sorted{$_} // 0) )
		for sort { $sort_list{$b} <=> $sort_list{$a} } keys %sort_list;
	say "";
	###


	my $td1 = timediff($t1, $t0);
	my $td2 = timediff($t2, $t1);
	my $td3 = timediff($t2, $t0);

	say "1st leg:",timestr($td1);
	say "2nd leg:", timestr($td2);
	say "TOTAL TIME:",timestr($td3);
}






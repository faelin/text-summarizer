package Text::Typifier;

use v5.14;
use List::AllUtils qw/ zip /;
use strict;
use warnings;
use utf8;

use vars qw(@ISA @EXPORT @EXPORT_OK %EXPORT_TAGS $VERSION);
require Exporter;

@ISA = qw(Exporter);
@EXPORT = qw( separate typify );
@EXPORT_OK = qw();
%EXPORT_TAGS = (all => [@EXPORT_OK]);
$VERSION = '1.0';



my $delineator = qr/\u002D\u2010-\u2015\u2212\uFE63\uFF0D.;:/ux;  #- ‐ ‒ – — ― ﹣ － . ; :
my $lt_brack = qr/\[\(\{\⟨\</ux;  #[ ( { ⟨ <
my $rt_brack = qr/\]\)\}\⟩\>/ux;  #] ) } ⟩ >

my $sen_init = qr/(?= [$lt_brack]? ["“]? [A-Z0-9] )/ux;
	#SENTENCE INITIATOR: detects beginning of a sentence (zero-width assertion)
my $sen_term = qr/[\.?!‽…⋯᠁]+  (?(?=["”]) (?: ["”](?= \h+ $sen_init )|(?:["”]\v))    |    [$rt_brack]?(?=(?:\s $sen_init )|\v|$))/ux;
	#SENTENCE TERMINATOR: matches any number of   [.?!] with optional final ["”]   followed by a new sentence or [\v] or the end of the block

my $flat_clause = qr/\w++   (?! [$rt_brack\.] \h+ )   (?> \h*[^$delineator,\s]+)*+/ux;
	#matches a grouping of words that    DOES NOT begin with a bullet    and    DOES NOT include [delineator] or [,]
my $list_clause = qr/\w++   (?! [$rt_brack\.] \h+ )   (?> \h*[^$delineator\s]+)*+/ux;
	#matches a grouping of words that    DOES NOT begin with a bullet    and    DOES NOT include [delineator]

my $linear_list = qr/(?: $list_clause[;]\h)++ $list_clause   |   (?: $flat_clause[,]\h)++ $flat_clause/ux;
	#matches several clauses in a row, delineated by   [;]   or   [,]    (n.b. clauses separated by [;] may have [,] interally)

my $complex_clause = qr/$linear_list   |   $flat_clause/ux;
	#matches either a   [linear_list]   or   [flat_clause]

my $sentence_list = qr/$complex_clause   [:]\h+   $linear_list/ux;
	#matches any   [complex_clause]   followed by a [:]   followed by a [linear_list]

my $sentence = qr/(?| $sentence_list   |   $complex_clause (?: [$delineator,] \h* $complex_clause)++)      (?: $sen_term)/ux;
    #matches either   a [sentence_list] followed by a sentence terminator   or   one or more delineated [complex_clause] followed by a sentence terminator

my $paragraph = qr/$sentence (?: \s+ $sentence)++/ux;
    #one or more   [sentence]   delineated by whitespace

my $title = qr/(*FAIL)/ux;
my $dateline = qr/(*FAIL)/ux;

my $dialog = qr/(*FAIL)/ux;


#THE FOLLOWING ARE PARAGRAPH-MATCHING BLOCKS FOR USE WHEN SEPARATING ADJACENT PARAGRAPHS WITHIN A TEXT
my $block_list = qr/(?P<list>
						^ (?P<indent>\h+)*+ 

						(?P<item>
							(?P<open>    [$lt_brack] (?P<space> \h+ )?   )?
							(?:    (?P<char>   (?P<alpha> [A-Za-z]++)  |  (?P<numer> \d++))  ([$delineator,]?\w+)*+    |    (?P<bullet> [^$lt_brack\w\s]++)  )
							(?P<close>     (?(<open>)    (?(<space>) \h+ | )  [$rt_brack]    |    (?(<char>)  (?:[$rt_brack]  |  \h*[$delineator,] )  |  )    ))
							\h* .+
						)

						(?P<line>
							\n{1,2} ^
							(?(?!\s+)
								(?(<open>) (?P=open) | )
								(?(<char>)   (?(<alpha>) [A-Za-z]+ | \d+ )([$delineator,]?\w+)*    |    (?: (?P=bullet)|(*FAIL) ))
								(?P=close)
							)
							\h* .+
						)*
					)/ux;
my $offset_block = qr/\h+ \V+  (?: \v ^ \h+ \w++ \V+)++/ux;
my $block_par = qr/(?: (?!\h) \V+ (?: \v (?!\h) \V+)*+ )/ux;
my $indent_par = qr/\h+ \V+  (?: \v (?!\h) \V+ | (?=\v(?:\v|\h|\Z)) )++/ux;
my $catch_all = qr/\h* \V+/ux;


my $paragraph_match = qr/( ^
						  (?: (?: $block_list )
						    | (?: $offset_block )
						    | (?: $block_par )
						    | (?: $indent_par )
						    | (?: $catch_all )
						  )
						  (?: \v{2,} | \v (?=\h) | \Z)
						)/mux;


my %formats = (
	#partial sentence
	clause => qr/$flat_clause/,

	#list of three or more items, delineated by [,;]
	linear_list => qr/$linear_list/,

	#one or more clauses, ending in   [:]   followed by a [linear_list]
	sentence_list => qr/$sentence_list/,

	#single alphanumeric chain followed by a delineating symbol    or    symbol followed by [\s]
	block_list => qr/$block_list/,

	#single complete sentence, must end in   [.?!]   or   ["”] followed by [\s][A-Z] or end of text
	sentence => qr/^$sentence$/,

	#one or more sentences, optionally ending in [:]
	paragraph => qr/$paragraph/,

	#sentence preceded by     one word or more words followed by a delineating symbol or [\s]
	dialog => qr/$dialog/,

	#fragment containing a date- or time-stamp
	dateline => qr/$dateline/,

	#fragment in all capitals or with trailing vertical whitespace
	title => qr/$title/,
);


use Benchmark ':hireswallclock';

sub separate {
	my $text  = shift;
	my @paragraphs;

	my $t0 = Benchmark->new;
	push @paragraphs => $1 while $text =~ m/$paragraph_match/gmuxs; #splits *text* into an array of paragraphs
	my $t1 = Benchmark->new;
	my $td = timediff($t1, $t0);
	say "Article took ",timestr($td);

	return @paragraphs;
}



sub typify {
	my $text = shift;
	my @paragraphs = separate $text;

	my @category;
	PARAGRAPH: for my $chunk (@paragraphs) {
		my @type;

		TEST: for my $format (keys %formats) {
			my $pattern = $formats{ $format };

			if ( $chunk =~ qr/$pattern/muxs ) {
				push @type => $format;
			}
		}
		push @type => 'fragment' unless scalar @type;
		push @category => \@type;
	}

	my @zipped = zip @paragraphs, @category;

			$DB::single = 1;

	return @zipped;
}




1;
__END__
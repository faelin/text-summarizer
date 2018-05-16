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
my $month = qr/Jan(uary)? | Feb(ruary)? | Mar(ch)? | Apr(il)? | May | Jun(e)? | Jul(y)? | Aug(ust)? | Sep(tember)? | Oct(ober)? | Nov(ember)? | Dec(ember)?/ux;
my $ordinal = qr/st | nd | rd | th/ux;

my $cap_word = qr/[A-Z][A-Za-z]++/ux;
my $name = qr/(?: (?: $cap_word \h | [A-Z]\. \h*)++ (?: (?|of|in|at|with|the|and|for) \h+)*+ )++ $cap_word/ux;
my $abbr = qr/([A-Z]\.){2,}/ux;
my $word = qr/$name|$abbr|['‘]?\w++ ['’\w-]*+/ux;
my $date = qr/(?| $month \h+ \d{1,2} $ordinal? ,? \h+ \d\d(\d\d)? 
				| \d{1,2} $ordinal? \h+ $month ,? \h+ \d\d(\d\d)? 

				| \d\d(\d\d)? \h+ $month \h+ \d{1,2} $ordinal? 

				| $month \d{1,2} $ordinal?

				| \d{1,2} $ordinal \h+ of \h+ $month

				| \d\d \/ \d\d \/ \d\d\d\d 
				| \d\d \- \d\d \- \d\d\d\d 
				| \d\d \. \d\d \. \d\d\d\d 

				| \d\d\d\d \/ \d\d \/ \d\d
				| \d\d\d\d \- \d\d \- \d\d
				| \d\d\d\d \. \d\d \. \d\d
			)/ux;

my $sen_init = qr/(?= [$lt_brack]? ["“]? (?: $cap_word|[0-9]++) )/ux;
	#SENTENCE INITIATOR: detects beginning of a sentence (zero-width assertion)
my $sen_term = qr/[\.?!‽…⋯᠁]+  (?(?=["”]) (?: ["”](?= \h+ $sen_init )|(?:["”]\v))    |    [$rt_brack]?  (?= (?:\s+ $sen_init )|\v|[^\w\d]) )/ux;
	#SENTENCE TERMINATOR: matches any number of   [.?!] with optional final ["”]   followed by a new sentence or [\v] or the end of the block

my $flat_clause = qr/$word++ (?: \h+ $word)*+/ux;
	#matches a grouping of words that

my $comma_clause = qr/$flat_clause ([,] \h+ $flat_clause)++/ux;

my $semicolon_list = qr/(?: $comma_clause;["”]?\h+)++ $comma_clause/ux;
	#matches several clauses in a row, delineated by   [;]   or   [,]    (n.b. clauses separated by [;] may have [,] interally)

my $complex_clause = qr/$semicolon_list   |   $comma_clause  |  $flat_clause/ux;
	#matches either a   [semicolon_list]   or   [flat_clause]

my $sentence_list = qr/$complex_clause   [:]\h+   $complex_clause/ux;
	#matches any   [complex_clause]   followed by a [:]   followed by a [complex_clause]

my $bracket_clause = qr/([$lt_brack] (?: $complex_clause | $sentence_list | (?R) )  [$rt_brack])/ux;

my $sentence = qr/(?| $sentence_list     |     (?: $complex_clause   (?: [$delineator,] \h* (?: $complex_clause|$sentence_list)?)++)  )      (?: $sen_term|  :(?=\v) )/ux;
    #matches either   a [sentence_list] followed by a sentence terminator   or   one or more delineated [complex_clause] followed by a sentence terminator

my $paragraph = qr/$sentence (?: \s+ $sentence)++/ux;
    #one or more   [sentence]   delineated by whitespace
my $title = qr/(*FAIL)/ux;
my $dateline = qr/(?| (?: $name , \h+)* $date | $date (?: , \h+ $name)+ ) /ux;

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


my %formats = (

	#grouping of words delineated by whitespace
	'10_flat_clause' => qr/$flat_clause/ux,

	#several clauses separated by commas
	'11_comma_clause' => qr/$comma_clause/ux,

	#list of three or more items, delineated by [,;]
	'20_semicolon_list' => qr/$semicolon_list/ux,

	#one or more clauses, ending in   [:]   followed by a [linear_list]
	'31_sentence_list' => qr/$sentence_list/ux,

	#any complex clause that opens and closes with a bracket
	'32_bracket_clause' => qr/$bracket_clause/ux,

	#single complete sentence, must end in   [.?!]   or   ["”] followed by [\s][A-Z] or end of text
	'40_sentence' => qr/$sentence/ux,

	#one or more sentences
	'50_paragraph' => qr/$paragraph/ux,

	#single alphanumeric chain followed by a delineating symbol    or    symbol followed by [\s]
	'60_block_list' => qr/$block_list/ux,

	#sentence preceded by     one word or more words followed by a delineating symbol or [\s]
	'70_dialog' => qr/$dialog/ux,

	#fragment containing a date- or time-stamp
	'80_dateline' => qr/$dateline/ux,

	#fragment in all capitals or with trailing vertical whitespace
	'81_title' => qr/$title/ux,

	#sequence of capitalized words    or    [A-Z] followed by a [.]
	'82_name' => qr/$name/ux,
);


use Benchmark ':hireswallclock';

sub separate {
	my $text  = shift;
	my @paragraphs;

	my $paragraph_match = qr/( ^
						  (?: (?: $block_list )
						    | (?: $offset_block )
						    | (?: $block_par )
						    | (?: $indent_par )
						    | (?: $catch_all )
						  )
						  (?: \v{2,} | \v (?=\h) | \Z)	
						)/mux;

	while ($text =~ m/$paragraph_match/gmuxs) {
		chomp( my $par = $1 );
		push @paragraphs => $par;
	}

	return @paragraphs;
}



sub typify {
	my $text = shift;
	my @paragraphs = separate $text;

	my @category;
	PARAGRAPH: for my $chunk (@paragraphs) {
		my @type;

		TEST: for my $format (sort keys %formats) {
			my $pattern = qr/$formats{ $format }/;
			my @scraps;

			while ( $chunk =~ m/($pattern)/gmuxs ) {
				push @scraps => $1;
			}
			push @type, ($format => \@scraps) if @scraps;
		}
		push @type, ('fragment' => \$chunk) unless @type;
		push @category => \@type;
	}

	my @zipped = zip @paragraphs, @category;

	return @zipped;
}




1;
__END__
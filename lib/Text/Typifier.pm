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
my $line_term = qr/(?= \v+ | \Z )/ux;
	#LINE TERMINATOR: detects the end of a single line or the end of the entire text (zero-width assertion)

my $indented = qr/^ \h+ \w++ \V+/ux;
	#matches any single indented line of text

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

my $title = qr//ux;
my $dateline = qr//ux;

my $dialog = qr//ux;



#THE FOLLOWING ARE PARAGRAPH-MATCHING BLOCKS FOR USE WHEN SEPARATING ADJACENT PARAGRAPHS WITHIN A TEXT
my $block_list = qr/(?P<list>
						^ (?P<indent>\h+)*+ 

						(?P<item>
							(?P<open>    [$lt_brack] (?P<space> \h+ )?   )?
							(?:    (?P<char>   (?P<alpha> [A-Za-z]+)  |  (?P<numer> \d+))  ([$delineator,]?\w+)*    |    (?P<bullet> [^$lt_brack\w\s]++)  )
							(?P<close>     (?(<open>)    (?(<space>) \h+ | )  [$rt_brack]    |    (?(<char>)  (?:[$rt_brack]  |  \h*[$delineator,] )  |  )    )     )
							\h*+ .+
						)

						(?P<line>
							\n{1,2} ^
							(?(?!\s+)
								(?(<open>) (?P=open) | )
								(?(<char>)   (?(<alpha>) [A-Za-z]+ | \d+ )([$delineator,]?\w+)*    |    (?: (?P=bullet)|(*FAIL) ))
								(?P=close)
							)
							\h*+ .+
						)*
					)/x;
my $indent_par = qr/$indented   $sen_term   $line_term/x;
my $offset_block = qr/$indented  (?: \v $indented)+  $line_term/x;
my $block_par = qr/(?: ^ \w+ \W \V+)  $line_term/x;
my $catch_all = qr/.+[\V]/x;




my %formats = (
	#partial sentence
	clause => $flat_clause,

	#list of three or more items, delineated by [,;]
	linear_list => $linear_list,

	#one or more clauses, ending in   [:]   followed by a [linear_list]
	sentence_list => $sentence_list,

	#single alphanumeric chain followed by a delineating symbol    or    symbol followed by [\s]
	block_list => $block_list,

	#single complete sentence, must end in   [.?!]   or   ["”] followed by [\s][A-Z] or end of text
	sentence => $sentence,

	#one or more sentences, optionally ending in [:]
	paragraph => $paragraph,

	#sentence preceded by     one word or more words followed by a delineating symbol or [\s]
	dialog => $dialog,

	#fragment containing a date- or time-stamp
	dateline => $dateline,

	#fragment in all capitals or with trailing vertical whitespace
	title => $title,
);




sub separate {
	my $text = shift;
	my $paragraph_match = qr/(?| $block_list
							   | $indent_par
							   | $offset_block
							   | $block_par
							   | $catch_all
							)/mx;
	my @paragraphs = $text =~ m/$paragraph_match/g; #splits *text* into an array of paragraphs

	return @paragraphs;
}



sub typify {
	my $text = shift;
	my @paragraphs = separate $text;

	my @category;
	PARAGRAPH: for $text (@paragraphs) {
		my @type;

		TEST: for my $format (keys %formats) {
			if ( $text =~ qr/^$formats{ $format }$/ ) {
				push @type => $format;
			} else {
				push @type => 'fragment';
			}
		}

		push @category => \@type;
	}

	return zip @paragraphs, @category;
}




1;
__END__
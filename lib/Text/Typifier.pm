package Text::Typifier;

use v5.14;
use List::AllUtils qw/ zip pairs pairwise /;
#use HTML::TreeBuilder 5 -weak;
use HTML::TreeBuilder::XPath;
use Text::Markup;
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
my $date = qr/(?(DEFINE)(?'month'$month))(?(DEFINE)(?'ordinal'$ordinal))
			  (?| (?P>month) \h+ \d{1,2} (?P>ordinal)? ,? \h+ \d\d(\d\d)? 
				| \d{1,2} (?P>ordinal)? \h+ (?P>month) ,? \h+ \d\d(\d\d)? 

				| \d\d(\d\d)? \h+ (?P>month) \h+ \d{1,2} (?P>ordinal)? 

				| (?P>month) \d{1,2} (?P>ordinal)?

				| \d{1,2} (?P>ordinal) \h+ of \h+ (?P>month)

				| \d\d \/ \d\d \/ \d\d\d\d 
				| \d\d \- \d\d \- \d\d\d\d 
				| \d\d \. \d\d \. \d\d\d\d 

				| \d\d\d\d \/ \d\d \/ \d\d
				| \d\d\d\d \- \d\d \- \d\d
				| \d\d\d\d \. \d\d \. \d\d
			  )/ux;

my $sen_init = qr/(?= [$lt_brack]? ["“]? (?: $cap_word|[0-9]++) )/ux;
	#SENTENCE INITIATOR: detects beginning of a sentence (zero-width assertion)
my $sen_term = qr/(?(DEFINE)(?'sen_init'$sen_init))
				  [\.?!‽…⋯᠁]+  (?(?=["”]) (?: ["”](?= \h+ (?P>sen_init) )|(?:["”]\v))    |    [$rt_brack]?  (?= (?:\s+ (?P>sen_init) )|\v|[^\w\d]) )/ux;
	#SENTENCE TERMINATOR: matches any number of   [.?!] with optional final ["”]   followed by a new sentence or [\v] or the end of the block

my $flat_clause = qr/(?(DEFINE)(?'word'$word))
					 (?P>word)++ (?: \h+ (?P>word))*+/ux;
	#matches a grouping of words that

my $comma_clause = qr/(?(DEFINE)(?'flat_clause'$flat_clause))
					  (?P>flat_clause) ([,] \h+ (?P>flat_clause))++/ux;

my $semicolon_list = qr/(?(DEFINE)(?'comma_clause'$comma_clause))
						(?: (?P>comma_clause);["”]?\h+)++ (?P>comma_clause)/ux;
	#matches several clauses in a row, delineated by   [;]   or   [,]    (n.b. clauses separated by [;] may have [,] interally)

my $complex_clause = qr/$semicolon_list   |   $comma_clause  |  $flat_clause/ux;
	#matches either a   [semicolon_list]   or   [flat_clause]

my $sentence_list = qr/(?(DEFINE)(?'complex_clause'$complex_clause))
					   (?P>complex_clause)   [:]\h+   (?P>complex_clause)/ux;
	#matches any   [complex_clause]   followed by a [:]   followed by a [complex_clause]

my $bracket_clause = qr/([$lt_brack] (?: $complex_clause | $sentence_list | (?R) )  [$rt_brack])/ux;

my $sentence = qr/(?(DEFINE)(?'complex_clause')$complex_clause)
				  (?| $sentence_list
                    | (?: (?P>complex_clause)   (?: [$delineator,] \h* (?: (?P>complex_clause)|$sentence_list)?)++)  
                  )
                  (?: $sen_term|  :(?=\v) )/ux;
    #matches either   a [sentence_list] followed by a sentence terminator   or   one or more delineated [complex_clause] followed by a sentence terminator

my $paragraph = qr/(?(DEFINE)(?'sentence'$sentence))(?P>sentence) (?: \s+ (?P>sentence))++/ux;
    #one or more   [sentence]   delineated by whitespace
my $title = qr/(*FAIL)/ux;
my $dateline = qr/(?| (?: $name , \h+)* $date | $date (?: , \h+ $name)+ ) /ux;

my $dialog = qr/(*FAIL)/ux;

my $html_bold = qr/<(?'tag' b)>  (?'text' .*?(?:((?R)).*?)?)  <\/\g{tag}>/ux;
my $html_italic = qr/<(?'tag' i)>  (?'text' .*?(?:((?R)).*?)?)  <\/\g{tag}>/ux;
my $html_strong = qr/<(?'tag' strong)>  (?'text' .*?(?:((?R)).*?)?)  <\/\g{tag}>/ux;
my $html_table = qr/<(?'tag' table)>  (?'text' .*?(?:((?R)).*?)?)  <\/\g{tag}>/ux;
my $html_faq_div = qr/<(?'tag' faq-\w+)>  (.*?(?:((?R)).*?)?)  <\/\g{tag}>/ux;
my $html_title = qr/<(?'tag' title)>  (?'text' .*?(?:((?R)).*?)?)  <\/\g{tag}>/ux;

#THE FOLLOWING ARE PARAGRAPH-MATCHING BLOCKS FOR USE WHEN SEPARATING ADJACENT PARAGRAPHS WITHIN A TEXT
my $html_block = qr/(<(?'tag'[\w-]+)(?:\h[^>]*)?>(.*?(?:(?'inner'(?R)).*?)?)<\/\g{tag}>)/ux;
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

	#matches text tagged with <b></b>
	'90_bold' => qr/$html_bold/ux,

	#matches text tagged with <i></i>
	'91_italic' => qr/$html_italic/ux,

	#matches text tagged with <strong></strong>
	'92_strong' => qr/$html_strong/ux,

	#matches text tagged with <table></table>
	'93_table' => qr/$html_table/ux,

	#matches text tagged with <faq-[...]></faq-[...]>
	'94_faq_div' => qr/$html_faq_div/ux,

	'95_title' => qr/$html_title/ux,
);


use Benchmark ':hireswallclock';

sub separate {
	my $tree = shift;
	my (@extracted, @nodes);

	@nodes = $tree->elementify->find('body')->detach_content;

	@extracted = extract(@nodes);

	my $paragraph_match = qr/((?: (?: $html_block )
								| (?: $block_list )
							    | (?: $offset_block )
							    | (?: $block_par )
							    | (?: $indent_par )
							    | (?: $catch_all )
							 )
							 (?: \v{2,} | \v (?=\h) | \Z))
							/mux;

	my @paragraphs;
	for my $chunk ( @extracted ) {
		while ($chunk =~ m/$paragraph_match/gmuxs) {
			chomp( my $par = $1 );

			push @paragraphs => $par;
		}
	}

	return @paragraphs;
}



		my $tracker = 0;

sub extract {
	my @nodes = @_;

	my @paragraphs;
	NODE: for my $node ( @nodes ) {
		my $tag = ($node->can('tag') ? $node->tag() // '' : '');

		if ( !$node->can('descendants') ) {
			my $text = $node->can('as_XML') ? $node->as_XML : $node;	
			push @paragraphs => $text unless $text =~ /^\s*$/;

		} elsif ( $tag eq 'table' ) {
			my @header = $node->find('thead');
			my @body = $node->find('tbody');
			my @footer = $node->find('tfoot');
			my $concat = '';
			for ( @header ) {
				$concat .= (join " " => map { $_->as_text } $node->find('th')) . "\n" for $node->find('tr');
			}
			for ( pairwise { $b ? ($a, $b) : ($a) } @body, @footer ) {
				$concat .= (join " " => map { $_->as_text } $_->find('td')) . "\n" for $node->find('tr');
			}
			push @paragraphs => "<$tag>$concat</$tag>";

		} elsif ( $tag eq 'ul' or $tag eq 'ol' ) {
			my $table_string = "<$tag>" . (join "\n" => map { $_->as_text } $node->find('li')) . "</$tag>";
			push @paragraphs => $table_string;

		} elsif ( $tag eq 'dl' ) {
			my $table_string = "<$tag>" . (join "\n" => map { join "\ " => ($_[0]->as_text, $_[1]->as_text) } pairs $node->find('dt', 'dd')) . "</$tag>";
			push @paragraphs => $table_string;

		} elsif ( $tag eq 'div' and (my $class = $node->attr('class')) =~ /^faq-\w+$/ ) {
			my @content = $node->detach_content;
			my $concat = '';
			for ( @content ) {
				if ( $_->can('descendants') and $_->descendants ) {
					$concat = "<$class>$concat</$class>";
					push @paragraphs => $concat;
					push @paragraphs => extract($_->detach_content);
					$concat = '';
				} else {
					$concat .= join " " => extract($_);
				}
			}
			$concat = "<$class>$concat</$class>";
			push @paragraphs => $concat;

		} elsif ( $node->find('code') ) {
			push @paragraphs => '<code>' . ( join " " => extract($node->content_list) ) . '</code>';

		} elsif ( $tag eq 'a' ) {
			my $parent = $node->parent;
			next NODE unless $parent;
			$node->replace_with_content;
			my @content = extract($parent);
			push @paragraphs => @content;

		} elsif (	$tag eq 'b' or 
					$tag eq 'i' or 
					$tag eq 'u' or 
					$tag eq 'strong' or 
					$tag eq 'title' or 
					$tag =~ /h\d/
				) {
			my @content = extract($node->content_list);
			$node->destroy_content if @	content;
			$node->push_content(@content);
			push @paragraphs => $node->as_XML;

		} elsif ( $tag eq 'code' ) {
			push @paragraphs => $node->as_text;

		} elsif ( $tag eq 'br' ) {
			$node->destroy;

		} else {
			push @paragraphs => extract($node->content_list);
		}
	}

	return @paragraphs;
}



sub typify {
	my $text = shift;

	open( my $temp, "+>:encoding(UTF-8)", "temp/raw.txt" ) or die "Can't open +> 'temp/raw.txt': $!";
	print $temp $text;
	close $temp;

	my $markup_parser = Text::Markup->new( default_format => 'markdown' );
	my $html = $markup_parser->parse(file => "temp/raw.txt");
	my $tree = HTML::TreeBuilder->new->parse_content($html);

	my @paragraphs = separate $tree;

	my @category;
	PARAGRAPH: for my $chunk (@paragraphs) {
		my @type;

		TEST: for my $format (sort keys %formats) {
			my $pattern = qr/$formats{ $format }/;
			my @scraps;

			while ( $chunk =~ m/($pattern)/gmuxs ) {
				push @scraps => $+{text_hint} // $1;
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
#!/usr/bin/env perl
use lib qw/ data lib /;
use Text::Summarizer;
use utf8;

use Test::More;
use List::AllUtils qw/ all all_u /;



my $summarizer = new_ok( Text::Summarizer => [ articles_path => "articles/*" ] );

subtest 'Datafile Attributes Set' => sub {
	my $paths = ['data/permanent.stop','data/stopwords.stop','data/watchlist.stop'];
	is $summarizer->permanent_path, $paths->[0] => "permanent path is '$paths->[0]'";
	is $summarizer->stopwords_path, $paths->[1] => "stopwords path is '$paths->[1]'";
	is $summarizer->watchlist_path, $paths->[2] => "watchlist path is '$paths->[2]'";
};


# $summarizer->scan_all;


my @summaries = $summarizer->summarize_all();
subtest '@summaries Structure Intact' => sub {
	if ( ok all_u( sub { ref $_ eq 'HASH' } => @summaries), 'summaries in hash form' ) {
		 ok all_u( sub { exists $_->{sentences} and exists $_->{fragments} and exists $_->{words} } => @summaries), 'summaries contain sentences, fragments, and words';

		 ok all_u( sub { all_u( sub { $_ > 0 } => values %{$_->{sentences}} ) } => @summaries ), 'all sentences scored';
		 ok all_u( sub { all_u( sub { $_ > 0 } => values %{$_->{fragments}} ) } => @summaries ), 'all fragments scored';
		 ok all_u( sub { all_u( sub { $_ > 0 } => values %{$_->{  words  }} ) } => @summaries ), 'all words are scored';

		 ok all_u( sub { all_u( sub { /(?<!\s[A-Z][a-z]) (?<!\s[A-Z][a-z]{2}) \. (?![A-Z]\.|\s[a-z0-9]) | \! | \? | \b$/x } => keys %{$_->{sentences}} ) } => @summaries ), 'sentences look sentency';
		 ok all_u( sub { all_u( sub { /(?: \( [\w'’-]+ (?: \| [\w'’-]+ )*  \) ) | (?: [\w'’-]+ (?: \s [\w'’-]+ )* )/x } => keys %{$_->{fragments}} ) } => @summaries ), 'fragments look fragmenty';
		 ok all_u( sub { all_u( sub { /[\w'’-]+/x } => keys %{$_->{  words  }} ) } => @summaries ), 'all words look like words';
	} else {
		fail 'summaries not in hash form';
	}
};



my $file_summ = $summarizer->summarize_file("articles/17900108-Washington.txt");
subtest '$file_summ Structure Intact' => sub {
	if ( ok ref $file_summ eq 'HASH', 'summaries in hash form' ) {
		 ok exists $file_summ->{sentences} and exists $file_summ->{fragments} and exists $file_summ->{words}, 'summaries contain sentences, fragments, and words';

		 ok all_u( sub { $_ > 0 } => values %{$file_summ->{sentences}} ), 'all sentences scored';
		 ok all_u( sub { $_ > 0 } => values %{$file_summ->{fragments}} ), 'all fragments scored';
		 ok all_u( sub { $_ > 0 } => values %{$file_summ->{  words  }} ), 'all words are scored';

		 ok all_u( sub { /(?<!\s[A-Z][a-z]) (?<!\s[A-Z][a-z]{2}) \. (?![A-Z]\.|\s[a-z0-9]) | \! | \? | \b$/x } => keys %{$file_summ->{sentences}} ), 'sentences look sentency';
		 ok all_u( sub { /(?: \( [\w'’-]+ (?: \| [\w'’-]+ )*  \) ) | (?: [\w'’-]+ (?: \s [\w'’-]+ )* )/x } => keys %{$file_summ->{fragments}} ), 'fragments look fragmenty';
		 ok all_u( sub { /[\w'’-]+/x } => keys %{$file_summ->{  words  }} ), 'all words look like words';
	} else {
		fail 'summaries not in hash form';
	}
};



my $some_text = <<'END_SAMPLE';
In publishing and graphic design, lorem ipsum is a filler text or greeking commonly used to demonstrate the textual elements of a graphic document or visual presentation. Replacing meaningful content with placeholder text allows designers to design the form of the content before the content itself has been produced.
The lorem ipsum text is typically a scrambled section of De finibus bonorum et malorum, a 1st-century BC Latin text by Cicero, with words altered, added, and removed to make it nonsensical, improper Latin.
A variation of the ordinary lorem ipsum text has been used in typesetting since the 1960s or earlier, when it was popularized by advertisements for Letraset transfer sheets. It was introduced to the Information Age in the mid-1980s by Aldus Corporation, which employed it in graphics and word-processing templates for its desktop publishing program PageMaker.
END_SAMPLE
my $text_summ = $summarizer->summarize_text($some_text);
subtest '$text_summ Structure Intact' => sub {
	if ( ok ref $text_summ eq 'HASH', 'summaries in hash form' ) {
		 ok exists $text_summ->{sentences} and exists $text_summ->{fragments} and exists $text_summ->{words}, 'summaries contain sentences, fragments, and words';

		 ok all_u( sub { $_ > 0 } => values %{$text_summ->{sentences}} ), 'all sentences scored';
		 ok all_u( sub { $_ > 0 } => values %{$text_summ->{fragments}} ), 'all fragments scored';
		 ok all_u( sub { $_ > 0 } => values %{$text_summ->{  words  }} ), 'all words are scored';

	binmode STDOUT, ":utf8";

		 ok all_u( sub { /(?<!\s[A-Z][a-z]) (?<!\s[A-Z][a-z]{2}) \. (?![A-Z]\.|\s[a-z0-9]) | \! | \? | \b$/x } => keys %{$text_summ->{sentences}} ), 'sentences look sentency';
		 ok all_u( sub { /(?: \( [\w'’-]+ (?: \| [\w'’-]+ )*  \) ) | (?: [\w'’-]+ (?: \s [\w'’-]+ )* )/x } => keys %{$text_summ->{fragments}} ), 'fragments look fragmenty';
		 ok all_u( sub { /[\w'’-]+/x } => keys %{$text_summ->{  words  }} ), 'all words look like words';
	} else {
		fail 'summaries not in hash form';
	}
};



done_testing();
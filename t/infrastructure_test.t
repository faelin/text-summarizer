#!/usr/bin/env perl
use lib qw/ data lib /;
use Text::Summarizer;

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

		 ok all_u( sub { all_u( sub { 1; } => keys %{$_->{sentences}} ) } => @summaries ), 'sentences look sentency';
		 ok all_u( sub { all_u( sub { /(?: \( [\w'’-]+ (?: \| [\w'’-]+ )+ \) ) | (?: [\w-]+ (?: \s [\w-]+ )+ )/x } => keys %{$_->{fragments}} ) } => @summaries ), 'fragments look fragmenty';
		 ok all_u( sub { all_u( sub { /[\w-]+/x } => keys %{$_->{  words  }} ) } => @summaries ), 'words look like words';
	} else {
		fail 'summaries not in hash form';
	}
};



# my $file_summ = $summarizer->summarize_file("articles/17900108-Washington.txt");
# my $text_summ = $summarizer->summarize_text(<<'END_SAMPLE');

# 	YOU don’t know about me without you have read a book by the name of The Adventures of Tom Sawyer; 
# 	but that ain’t no matter. That book was made by Mr. Mark Twain, and he told the truth, mainly. There 
# 	was things which he stretched, but mainly he told the truth. That is nothing. I never seen anybody 
# 	but lied one time or another, without it was Aunt Polly, or the widow, or maybe Mary. Aunt 
# 	Polly—Tom’s Aunt Polly, she is—and Mary, and the Widow Douglas is all told about in that book, 
# 	which is mostly a true book, with some stretchers, as I said before. Now the way that the book winds 
# 	up is this: Tom and me found the money that the robbers hid in the cave, and it made us rich. We got 
# 	six thousand dollars apiece—all gold. It was an awful sight of money when it was piled up. Well, 
# 	Judge Thatcher he took it and put it out at interest, and it fetched us a dollar a day apiece all 
# 	the year round—more than a body could tell what to do with. The Widow Douglas she took me for her 
# 	son, and allowed she would sivilize me; but it was rough living in the house all the time, 
# 	considering how dismal regular and decent the widow was in all her ways; and so when I couldn’t 
# 	stand it no longer I lit out. I got into my old rags and my sugar-hogshead again, and was free and 
# 	satisfied. But Tom Sawyer he hunted me up and said he was going to start a band of robbers, and I 
# 	might join if I would go back to the widow and be respectable. So I went back. The widow she cried 
# 	over me, and called me a poor lost lamb, and she called me a lot of other names, too, but she never 
# 	meant no harm by it. She put me in them new clothes again, and I couldn’t do nothing but sweat and 
# 	sweat, and feel all cramped up.
# END_SAMPLE


# $summarizer->pretty_print($file_summ, 20);



done_testing();
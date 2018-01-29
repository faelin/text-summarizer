#!/usr/bin/env perl
use lib 'lib';
use Text::Summarizer;

my $summarizer = Text::Summarizer->new(articles_path => "articles/*");

my $new_words = $summarizer->scan_all();

my @summaries = $summarizer->summarize_all();
my $file_summ = $summarizer->summarize_file("articles/17900108-Washington.txt");
my $text_summ = $summarizer->summarize_text(<<'END_SAMPLE');

	YOU don’t know about me without you have read a book by the name of The Adventures of Tom Sawyer; 
	but that ain’t no matter. That book was made by Mr. Mark Twain, and he told the truth, mainly. There 
	was things which he stretched, but mainly he told the truth. That is nothing. I never seen anybody 
	but lied one time or another, without it was Aunt Polly, or the widow, or maybe Mary. Aunt 
	Polly—Tom’s Aunt Polly, she is—and Mary, and the Widow Douglas is all told about in that book, 
	which is mostly a true book, with some stretchers, as I said before. Now the way that the book winds 
	up is this: Tom and me found the money that the robbers hid in the cave, and it made us rich. We got 
	six thousand dollars apiece—all gold. It was an awful sight of money when it was piled up. Well, 
	Judge Thatcher he took it and put it out at interest, and it fetched us a dollar a day apiece all 
	the year round—more than a body could tell what to do with. The Widow Douglas she took me for her 
	son, and allowed she would sivilize me; but it was rough living in the house all the time, 
	considering how dismal regular and decent the widow was in all her ways; and so when I couldn’t 
	stand it no longer I lit out. I got into my old rags and my sugar-hogshead again, and was free and 
	satisfied. But Tom Sawyer he hunted me up and said he was going to start a band of robbers, and I 
	might join if I would go back to the widow and be respectable. So I went back. The widow she cried 
	over me, and called me a poor lost lamb, and she called me a lot of other names, too, but she never 
	meant no harm by it. She put me in them new clothes again, and I couldn’t do nothing but sweat and 
	sweat, and feel all cramped up.
END_SAMPLE


#$summarizer->pretty_print($file_summ, 50);

#$summarizer->pretty_print($_) for @summaries;


# open( my $file, '>>', sprintf( "data/phrases.csv", $i) )
# 		or die "Can't open data/phrases.csv: $!";

# my $phrases = $summarizer->phrase_list;
# my @phrase_list = sort { $phrases->{$b} <=> $phrases->{$a} } keys %$phrases;
# my $highest = $phrase_list[0];
# for my $sen ( sort { $phrases->{$b} <=> $phrases->{$a} } keys %$phrases ) {
# 	my $phrase = join ' ' => split /[^A-Za-z0-9-']+/ => $sen;
# 	my $score = $phrases->{$sen} / $phrases->{$highest};
# 	# print $file "$phrase,$score\n";
# }

# close $file;
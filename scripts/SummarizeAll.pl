#!/usr/bin/env perl
use Text::Summarizer;

my $summarizer = Text::Summarizer->new(
		articles_path => 'tickets/*',
		permanent_path => 'data/summarizer/permanent.stop',
		stopwords_path => 'data/summarizer/stopwords.stop',
		watchlist_path => 'data/summarizer/watchlist.stop',
	);

# $summarizer->scan_all;
$summarizer->summarize_all;

open( my $file, '>>', sprintf( "data/phrases.csv", $i) )
		or die "Can't open data/phrases.csv: $!";

my $phrases   = $summarizer->phrase_list;
my @phrase_list = sort { $phrases->{$b} <=> $phrases->{$a} } keys %$phrases;
my $highest = $phrase_list[0];
for my $sen ( sort { $phrases->{$b} <=> $phrases->{$a} } keys %$phrases ) {
	my $phrase = join ' ' => split /[^A-Za-z0-9-']+/ => $sen;
	my $score = $phrases->{$sen} / $phrases->{$highest};
	print $file "$phrase,$score\n";
}

close $file;
#!/usr/bin/env perl
use Text::Summarizer;

my $summarizer = Text::Summarizer->new(
		articles_path => 'articles/*',
		permanent_path => 'data/permanent.stop',
		stopwords_path => 'data/stopwords.stop',
		watchlist_path => 'data/watchlist.stop',
	);

$summarizer->scan_all;
$summarizer->summarize_all;
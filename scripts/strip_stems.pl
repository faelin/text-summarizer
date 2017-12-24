#!/usr/bin/perl
use strict;
use warnings;
use Lingua::Stem::Snowball qw/ stem /;


open( my $text_file, '<', "data/ts_thesaurus.ths" )
	or die "Can't open data/ts_thesaurus.ths: $!";

my @lines = grep { $_ } <$text_file>;
close $text_file;

open( $text_file, '>', "data/ts_thesaurus.ths" )
	or die "Can't open data/ts_thesaurus.ths: $!";


for my $line (@lines) {
	chomp $line;
	my @tokens = split / / => $line;
	@tokens = stem( 'en', \@tokens );

	say $text_file join ' ' => @tokens;
}

close $text_file;
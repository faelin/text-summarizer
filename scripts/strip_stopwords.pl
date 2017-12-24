#!/usr/bin/perl
use strict;
use warnings;


open( my $stop_file, '<', "data/ts_stopwords.stop" )
	or die "Can't open data/ts_stopwords.stop: $!";
open( my $text_file, '<', "data/ts_thesaurus.ths" )
	or die "Can't open data/ts_thesaurus.ths: $!";

my $text = join "" => map { $_ } <$text_file>;
close $text_file;

open( $text_file, '>', "data/ts_thesaurus.ths" )
	or die "Can't open data/ts_thesaurus.ths: $!";


for my $word (<$stop_file>) {
	chomp $word;
	$text =~ s/(\n| )$word /$1\? /gi;
}
close $stop_file;

print $text_file $text;
close $text_file;




sub DEBUG {
	my $msg = shift;
	my $time = localtime();

	open(our $logfile, '>>', '/var/web/logs/reducer/stopwords.log') or die $!;
	say $logfile "[$time] [$$] $msg";
	warn "log written: $msg";
	close $logfile;
}
sub KILL {
	my $msg = shift;
	my $time = localtime();

	open(our $logfile, '>>', '/var/web/logs/reducer/stopwords.log') or die $!;
	say $logfile "[$time] [$$] $msg";
	say $logfile "\n";
	warn "log written: $msg";
	die "Program Terminated: $!".shortmess();
	close $logfile;
}
sub TRACE {
	my $msg = shift;
	my $time = localtime();

	open(our $logfile, '>>', '/var/web/logs/reducer/stopwords.log') or die $!;
	say $logfile "[$time] [$$] $msg";
	say $logfile longmess();
	say $logfile "\n";
	warn "log written: $msg";
	close $logfile;
}

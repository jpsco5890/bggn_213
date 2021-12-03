#!/usr/bin/perl -w
use strict;

my(@lines);

open(INFILE, $ARGV[0]);
chomp(@lines = <INFILE>);
close(INFILE);

open(OUTFILE, ">" . $ARGV[1]);
foreach $_ (@lines){
	if(/>.*/){
		s/>((?:\D)+_(?:\d)+\.\d):{0,1}?.+\[(.+)\]/\n>$2 \[$1\]\n/;
		print OUTFILE;
	}
	else{
		print OUTFILE ("$_");
	}
}
close(OUTFILE);

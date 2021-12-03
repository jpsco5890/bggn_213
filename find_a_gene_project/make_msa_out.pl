#!/usr/bin/perl
use strict;

my($i, @lines);

open(INFILE, $ARGV[0]);
chomp(@lines = <INFILE>);
close(INFILE);

open(OUTFILE, ">" . $ARGV[1]);

for($i = 0; $i <= $#lines; $i += 2){
	$_ = $lines[$i+1];
	s/(.{0,35})(.*)/$2/;
	printf OUTFILE ("%49s  %-35s\n", $lines[$i], $1);
	$lines[$i+1] = $_;
}

print OUTFILE ("\n");

while($lines[1] ne ""){
	for($i = 0; $i <= $#lines; $i += 2){
		$_ = $lines[$i];
		s/.+\[(.+)\]/$1/;
		printf OUTFILE ("%-14s  ", $_);
		$_ = $lines[$i+1];
		s/(.{0,70})(.*)/$2/;
		print OUTFILE ("$1\n");
		$lines[$i+1] = $_;
	}
	print OUTFILE ("\n");
}

close(OUTFILE);

#!/usr/bin/perl

print "Bin to POKE converter for MikeBASIC\n";
print "by Paulo Valongo 26 September 2011\n";
print "    \n";
print "Enter name and path to bin file\n";
$FileIn = <>;
print "Enter name and path to text file to be created\n";
$FileOut = <>;
print "Enter start POKE address in decimal\n";
$PokeAddr = <>;

chomp($FileOut);
chomp($PokeAddr);
$OrigPokeAddr = $PokeAddr;

open (FILE1, ">", $FileOut) or die $!;

open (FILE2, $FileIn) or die $!;
binmode FILE2;
my ($data, $n);

while (($n = read FILE2, $data, 1) != 0)
{
	$ord = ord($data);
	print FILE1 "poke ", $ord, " ", $PokeAddr, "\n";
	$PokeAddr = $PokeAddr + 1;
}

print FILE1 "call ", $OrigPokeAddr;
close FILE2;
close FILE1;
$NumBytes = $PokeAddr - $OrigPokeAddr;
print "DONE, processed ", $NumBytes, " bytes\n";

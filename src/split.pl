#!/usr/bin/perl

undef $/;
$_ = <>;
$n = 0;

for $match (split(/(?=module MDL)/)) {
      open(O, '>mdl' . ++$n . '.purs');
      print O $match;
      close(O);
}

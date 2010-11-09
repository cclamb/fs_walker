#/usr/bin/perl
use File::Path;
use strict;
use warnings;

my %paths = (
             '/scratch1/raballa/' => 0,
             '/scratch1/raballa/dir.1' => 10000,
             '/scratch1/raballa/dir.2' => 20000,
             '/scratch1/raballa/dir.3' => 30000,
             '/scratch1/raballa/dir.4' => 40000,
             '/scratch1/raballa/dir.5' => 50000,
             '/scratch1/raballa/dir.10' => 100000,
             );

sub populate {
    my $dirname = shift;
    my $file_count = shift;
    mkpath($dirname, 1, 0700);

    printf ("File count is %d\n", $file_count);
    for (my $i = 0; $i < $file_count; $i++) {
        my $filename = sprintf("%s/file%06d", $dirname, $i);
        `/usr/bin/touch $filename`;
    }
}

foreach my $key (keys %paths) {
    populate($key, $paths{$key});
}



            
    
             
    



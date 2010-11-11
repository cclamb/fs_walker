#/usr/bin/perl
use File::Path;
use strict;
use warnings;

my $ROOT = "/gscratch2/raballa";

my %paths = (
             '/' => 0,
#             '/dir.1' => 10000,
#             '/dir.2' => 20000,
#             '/dir.3' => 30000,
#             '/dir.4' => 40000,
#             '/dir.5' => 50000,
             '/dir.10' => 100000,
 #            '/dir.tree' => 1000,

#              '/dir.tree/1' => 200,
#              '/dir.tree/1/1' => 20000,
#              '/dir.tree/1/2' => 20000,
#              '/dir.tree/1/3' => 20000,
#              '/dir.tree/1/4' => 20000,
#              '/dir.tree/1/5' => 20000,

#             '/dir.tree/2' => 200,
#             '/dir.tree/2/1' => 20000,
             '/dir.tree/2/2' => 20000,
#             '/dir.tree/2/3' => 20000,
#             '/dir.tree/2/4' => 20000,
             '/dir.tree/2/5' => 20000,

#             '/dir.tree/3' => 2000,
             '/dir.tree/3/1' => 20000,
#             '/dir.tree/3/2' => 20000,
             '/dir.tree/3/3' => 20000,
#             '/dir.tree/3/4' => 20000,
             '/dir.tree/3/5' => 20000,

#             '/dir.tree/4' => 2000,
#             '/dir.tree/4/1' => 20000,
#             '/dir.tree/4/2' => 20000,
             '/dir.tree/4/3' => 20000,
#             '/dir.tree/4/4' => 20000,
#             '/dir.tree/4/5' => 20000,

#             '/dir.tree/5' => 2000,
             '/dir.tree/5/1' => 20000,
#             '/dir.tree/5/2' => 20000,
#             '/dir.tree/5/3' => 20000,
#             '/dir.tree/5/4' => 20000,
#             '/dir.tree/5/5' => 20000,

             );

sub populate {
    my $dirname = shift;
    my $file_count = shift;
    mkpath($dirname, 1, 0700);

    printf ("%s: File count is %d\n", $dirname, $file_count);
    for (my $i = 0; $i < $file_count; $i++) {
        my $filename = sprintf("%s/file%06d", $dirname, $i);
        `/bin/touch $filename`;
    }
    printf ("%s: complete\n", $dirname);
}

foreach my $key (keys %paths) {
    my $dirname = sprintf("%s/%s", $ROOT, $key);
    populate($dirname, $paths{$key});
}



            
    
             
    



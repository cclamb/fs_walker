#!/usr/bin/perl -w

my $NODENAME = "worker";
my @hosts = ();
my $i = 0;
open(HOSTS, "pdsh hostname |");
while (<HOSTS>) {
    chomp;
    my ($host, $host2) = split(/:\s+/);
    if ($host) {
        $hosts[$i++] = sprintf('%s@%s', $NODENAME, $host);
    }
}

printf("[%s].\n", join(",\n", @hosts));


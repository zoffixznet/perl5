#!/usr/bin/perl -w
use strict;
use Test::More tests => 4;
use constant NO_SUCH_FILE => "this_file_had_better_not_exist";
use File::Temp qw(tempfile);
use autodie;

eval { utime(undef, undef, NO_SUCH_FILE); };
isa_ok($@, 'autodie::exception', 'exception thrown for utime');

my ($atime, $mtime);
my ($tmpfh, $tmpfile);
SKIP: {
    # Some systems have a screwy tempfile. We don't run our tests there.

    local $@;
    eval { ($tmpfh, $tmpfile) = tempfile(UNLINK => 1); };
    my $reason = 'tempfile() not happy on this system.';
    skip $reason, 3 if ($@ or !defined $tmpfh);

    eval { utime(undef, undef, $tmpfile); };
    ok(! $@, "We can utime a tempfile just fine.") or diag $@;

    eval { utime(undef, undef, NO_SUCH_FILE, $tmpfile); };
    isa_ok($@, 'autodie::exception', 'utime exception on single failure.');
    is($@->return, 1, "utime fails correctly on a 'true' failure.");
}

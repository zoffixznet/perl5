#!/usr/bin/perl -w
use strict;
use Test::More;
use constant NO_SUCH_FILE => "this_file_had_better_not_exist";
use File::Temp qw(tempfile);
use autodie;

if ($^O eq 'MSWin32') {
    plan skip_all => 'chown() seems to always succeed on Windows';
}

plan tests => 4;

eval {
    chown(1234, 1234, NO_SUCH_FILE);
};

isa_ok($@, 'autodie::exception', 'exception thrown for chown');

# Chown returns the number of files that we chowned. So really we
# should die if the return value is not equal to the number of arguments
# minus two.

my ($tmpfh, $tmpfile);
SKIP: {
    # Some systems have a screwy tempfile. We don't run our tests there.

    local $@;
    eval { ($tmpfh, $tmpfile) = tempfile(UNLINK => 1); };
    my $reason = 'tempfile() not happy on this system.';
    skip $reason, 3 if ($@ or !defined $tmpfh);

    eval { chown($<, -1, $tmpfile); };
    ok(! $@, "Can chown a tempfile just fine.");

    eval { chown($<, -1, $tmpfile, NO_SUCH_FILE); };
    isa_ok($@, 'autodie::exception', "Exception if ANY file changemode fails");
    is($@->return, 1, "Confirm we're dying on a 'true' chown failure.");
}

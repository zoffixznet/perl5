#!/usr/bin/perl -w
use strict;
use Test::More tests => 7;
use constant NO_SUCH_FILE => "this_file_had_better_not_exist";
use constant ERROR_REGEXP => qr{Can't chmod\(0755, '${\(NO_SUCH_FILE)}'\):};
use constant SINGLE_DIGIT_ERROR_REGEXP => qr{Can't chmod\(0010, '${\(NO_SUCH_FILE)}'\):};
use File::Temp qw(tempfile);
use autodie;

# This tests RT #50423, Debian #550462

eval { chmod(0755, NO_SUCH_FILE); };
isa_ok($@, 'autodie::exception', 'exception thrown for chmod');
like($@, ERROR_REGEXP, "Message should include numeric mode in octal form");

eval { chmod(8, NO_SUCH_FILE); };
isa_ok($@, 'autodie::exception', 'exception thrown for chmod');
like($@, SINGLE_DIGIT_ERROR_REGEXP, "Message should include numeric mode in octal form");

my ($tmpfh, $tmpfile);
SKIP: {
    # Some systems have a screwy tempfile. We don't run our tests there.

    local $@;
    eval { ($tmpfh, $tmpfile) = tempfile(UNLINK => 1); };
    my $reason = 'tempfile() not happy on this system.';
    skip $reason, 3 if ($@ or !defined $tmpfh);

    eval { chmod(0755, $tmpfile); };
    ok(! $@, "We can chmod a tempfile just fine.");

    eval { chmod(0755, $tmpfile, NO_SUCH_FILE) };
    isa_ok($@, 'autodie::exception', 'chmod exception on any file failure.');
    is($@->return,1,"Confirm autodie on a 'true' chown failure.");
}

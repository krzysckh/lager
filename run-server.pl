#!/usr/bin/perl

use strict;
use warnings;

use Privileges::Drop;

drop_privileges('nobody');

while (1) {
  if (fork == 0) {
    exec("/usr/local/bin/ol-rl", "-r", "lager-server.scm");
  } else {
    wait
  }
}

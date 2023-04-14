#!/bin/sh

# Inspired by https://gist.github.com/primeapple/d3d82fbd28e9134d24819dd72430888e

source ~/.env

log_file=~/.task/sync.log

is_up() {
  if ! nc -z $TASKD_SERVER $TASKD_PORT; then
    echo "Server is down" >> $log_file
    exit 1
  fi
}

is_up
date > $log_file
task rc.verbose:nothing sync >> $log_file &

exit 0

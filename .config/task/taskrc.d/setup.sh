#!/usr/bin/env bash

# Evaluate environment variables into `server` config file

source ~/.env

config_file=~/.config/task/taskrc.d/server

echo "taskd.certificate=$XDG_DATA_HOME/task/default-client.cert.pem" > $config_file
echo "taskd.key=$XDG_DATA_HOME/task/default-client.key.pem" >> $config_file
echo "taskd.ca=$XDG_DATA_HOME/task/ca.cert.pem" >> $config_file
echo "taskd.trust=ignore hostname" >> $config_file
echo "taskd.server=$TASKD_SERVER:$TASKD_PORT" >> $config_file
echo "taskd.credentials=$TASKD_CREDS" >> $config_file

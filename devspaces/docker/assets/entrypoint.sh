#!/bin/bash

# Immediately exits on error
set -o errexit

# Startup managed services in background and blocks execution
# (we need runit to bootstrap NGINX)
/sbin/my_init

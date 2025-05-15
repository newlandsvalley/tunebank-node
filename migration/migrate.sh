#!/bin/bash
################################################
#
# migrate musicrest data into tunebank node
#
# usage: migrate.sh users
# usage: migrate.sh scandi tunes
# usage: migrate.sh english comments
# etc.
#
#################################################

target=$1
genre=$2

node migrate.js $target $genre

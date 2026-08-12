#!/bin/bash
# Copyright (C) 2026 Gornskew Enterprises
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.  Distributed WITHOUT
# ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.


set -e

OS="$1"

task="$2"

script="$3"

SCRIPT=`realpath $0`

status="$(curl -s "http://localhost:9000/build?command=build-app&os=$OS&branch=$CI_COMMIT_REF_NAME&task=$2&script=$script&ci-project-dir=$CI_PROJECT_DIR")"

echo "Testing status of curl command for os=${1}, task=${2}, SCRIPT=${SCRIPT}..."

echo "CI_BUILDS_DIR is $CI_BUILDS_DIR"
echo "CI_PROJECT_DIR is $CI_PROJECT_DIR"

if [ "${status}" == "0" ]
then
    echo "Success."
    exit 0
else
    echo "Fail."
    echo "${status}" >&2
    exit 1
fi



#!/bin/bash
set -e

time docker-compose build --pull

time docker-compose run --rm ce-bfh bash delete-all-data.sh

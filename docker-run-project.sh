#!/bin/bash
set -e

time docker build --pull -t ce-bfh:4.2.2 .

time docker-compose run --rm ce-bfh Rscript run_project.R

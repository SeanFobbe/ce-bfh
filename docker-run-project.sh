#!/bin/bash
set -e

time docker-compose build --pull

time docker-compose run --rm ce-bfh Rscript run_project.R

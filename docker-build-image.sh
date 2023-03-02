#!/bin/bash
set -e

time docker build --pull -t ce-bfh:4.2.2 .

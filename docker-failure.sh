#!/usr/bin/env bash
set -Eeuxo pipefail

Rscript -e "installed.packages()[, \"Version\", drop = TRUE]"
env | sort
df -h | sort
ls -al
docker images
docker system prune --all --force
docker images
df -h | sort

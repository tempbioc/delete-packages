#!/bin/bash -l
set -e
echo "Synchronizing ${1} with ${2}"
Rscript -e "deletepackages::delete_from_server('${1}','${2}')"
echo "Action complete!"

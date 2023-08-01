#!/bin/bash -l
set -e
echo "Synchronizing ${1}"
Rscript -e "deletepackages::delete_from_server('${1}')"
echo "Action complete!"

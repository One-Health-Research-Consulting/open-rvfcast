#!/usr/bin/env bash
## Run the full pipeline end to end: predictor data (base) -> RVF response data (rvf) -> model (forecast)
set -euo pipefail
cd "$(dirname "$0")"

sed -i.bak -E "s/^TAR_PROJECT=.*/TAR_PROJECT=base/" .env
Rscript -e 'targets::tar_make(pipeline_complete)'

sed -i.bak -E "s/^TAR_PROJECT=.*/TAR_PROJECT=rvf/" .env
Rscript -e 'targets::tar_make(pipeline_complete)'

## Set the model pipeline purpose (match the PURPOSE line itself, not TAR_PROJECT)
sed -i.bak -E "s/^PURPOSE=.*/PURPOSE=forecast/" .env
sed -i.bak -E "s/^TAR_PROJECT=.*/TAR_PROJECT=model/" .env
Rscript -e 'targets::tar_make(pipeline_complete)'

## Remove the backup copy of .env that sed -i.bak leaves behind
rm -f .env.bak

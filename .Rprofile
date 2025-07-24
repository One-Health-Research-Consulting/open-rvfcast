source("renv/activate.R")
# Load env vars from any file starting with `.env`. This allows user-specific
# options to be set in `.env_user` (which is .gitignored), and to have both
# encrypted and non-encrypted .env files
load_env <- function(){
  for (env_file in list.files(all.files = TRUE, pattern = "^\\.env.*")) {
    try(readRenviron(env_file), silent = TRUE)
  }
}
load_env()

# If there is a bucket, cache targets remotely. Otherwise, do so locally.
if(!nzchar(Sys.getenv("TAR_PROJECT"))) {
  if(nzchar(Sys.getenv("AWS_BUCKET_ID"))) {
    Sys.setenv(TAR_PROJECT = "s3")
  } else {
    Sys.setenv(TAR_PROJECT = "main")
  }
}

# Set options for renv convenience
options(
  repos = c(CRAN = "https://cloud.r-project.org",
            ROPENSCI = "https://ropensci.r-universe.dev"),
  renv.config.auto.snapshot = FALSE, ## Attempt to keep renv.lock updated automatically
  renv.config.rspm.enabled = TRUE, ## Use RStudio Package manager for pre-built package binaries for linux
  renv.config.install.shortcuts = FALSE, ## Use the existing local library to fetch copies of packages for renv
  renv.config.cache.enabled = FALSE   ## Use the renv build cache to speed up install times
)

# Set options for internet timeout
options(timeout = max(300, getOption("timeout")))

# If project packages have conflicts define them here so as
# as to manage them across all sessions when building targets
if(requireNamespace("conflicted", quietly = TRUE)) {
  conflicted::conflict_prefer("filter", "dplyr", quiet = TRUE)
  conflicted::conflict_prefer("count", "dplyr", quiet = TRUE)
  conflicted::conflict_prefer("select", "dplyr", quiet = TRUE)
  conflicted::conflict_prefer("set_names", "magrittr", quiet = TRUE)
  conflicted::conflict_prefer("View", "utils", quiet = TRUE)
}

if(interactive()){
  message(paste("targets project is", Sys.getenv("TAR_PROJECT")))
  require(targets)
  require(tidyverse)
}

if (interactive() && Sys.getenv("TERM_PROGRAM") == "vscode") {

  options(vsc.dev.args = list(
    width = 1500,
    height = 1500,
    pointsize = 12,
    res = 300
  ))

  if (requireNamespace("httpgd", quietly = TRUE)) {
    options(vsc.plot = FALSE)
    options(device = function(...) {
      httpgd::hgd(silent = TRUE)
      .vsc.browser(httpgd::hgd_url(history = FALSE), viewer = "Beside")
    })
  }
}

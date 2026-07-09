source("renv/activate.R")
# Derive the project root from R_PROFILE_USER when set (e.g. crew workers that
# inherit it via Sys.setenv in packages.R), otherwise assume cwd is project root
local({
  renv_script <- if (nzchar(Sys.getenv("R_PROFILE_USER"))) {
    file.path(dirname(Sys.getenv("R_PROFILE_USER")), "renv/activate.R")
  } else {
    "renv/activate.R"
  }
  if (file.exists(renv_script)) source(renv_script)
})
library(lubridate)
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
  conflicted::conflicts_prefer(
    dplyr::filter,
    dplyr::count,
    dplyr::select,
    magrittr::set_names,
    utils::View,
    .quiet = TRUE
  )
}

if(interactive()){
  message(paste("targets project is", Sys.getenv("TAR_PROJECT")))
  require(targets)
  require(tidyverse)
}

if (interactive() && Sys.getenv("TERM_PROGRAM") == "vscode") {
  if (requireNamespace("httpgd", quietly = TRUE)) {
    # Use httpgd for VS Code's plot viewer (avoids XQuartz entirely)
    options(vsc.plot = FALSE)
    options(device = function(...) {
      httpgd::hgd(silent = TRUE)
      .vsc.browser(httpgd::hgd_url(history = FALSE), viewer = "Beside")
    })
  } else {
    # Fall back to native macOS quartz to avoid XQuartz focus-stealing behavior
    options(device = "quartz")
  }
}

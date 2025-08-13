#' Get Project Package Status with Usage Flags
#'
#' Compares the project's lockfile and library state and returns a tibble with
#' flags for presence in the lockfile, library, consistency, and whether the
#' package is used by the project (as reported by `renv::dependencies()`).
#'
#' @return A tibble with columns: `package`, `lockfile`, `library`, `consistent`, `project_dependency`
#' @export
project_packages_status_tbl <- function() {
  status <- renv::status()
  lockfile_pkgs <- names(status$lockfile$Packages)
  library_pkgs <- names(status$library)
  used_pkgs <- unique(renv::dependencies()$Package)

  all_pkgs <- union(union(lockfile_pkgs, library_pkgs), used_pkgs)

  tibble::tibble(
    package = all_pkgs,
    lockfile = as.integer(all_pkgs %in% lockfile_pkgs),
    library = as.integer(all_pkgs %in% library_pkgs),
    consistent = as.integer(all_pkgs %in% lockfile_pkgs & all_pkgs %in% library_pkgs &
                              !vapply(all_pkgs, function(pkg) {
                                !is.null(status$library[[pkg]]) &&
                                  !is.null(status$lockfile$Packages[[pkg]]) &&
                                  identical(
                                    status$library[[pkg]]$Version,
                                    status$lockfile$Packages[[pkg]]$Version
                                  )
                              }, logical(1))),
    project_dependency = as.integer(all_pkgs %in% used_pkgs)
  )
}

#' Classify renv Install Errors
#'
#' Given an error message string, return a classification for the error.
#'
#' @param msg Error message string
#' @return A character string with the error category
#' @keywords internal
classify_error <- function(msg) {
  msg <- tolower(msg)
  if (grepl("dependency.*not available", msg)) return("Missing R package dependency")
  if (grepl("not found", msg) && grepl("/usr|brew|apt|yum|library", msg)) return("Missing system dependency")
  if (grepl("compilation.*error|compile.*failed|g\\+\\+", msg)) return("Compilation error")
  return("Unknown")
}

#' Stepwise Restore Using renv
#'
#' Attempts to incrementally restore all packages that are both inconsistent and
#' actually used by the project (as determined by `renv::dependencies()`).
#' It avoids redundant restores and classifies errors.
#'
#' @param upgrade_failed Logical. Whether to attempt `renv::update()` on packages
#'   that failed to restore before retrying.
#' @return A tibble of failed package restores with error messages and categories.
#' @export
stepwise_restore <- function(upgrade_failed = TRUE) {
  used_packages <- unique(renv::dependencies()$Package)
  status_tbl <- project_packages_status_tbl()

  inconsistent_pkgs <- status_tbl |>
    dplyr::filter(consistent == 0, project_dependency == 1) |>
    dplyr::pull(package)

  message("The following package(s) are inconsistent and used by the project:\n")
  print(inconsistent_pkgs)

  error_log <- list()
  failed <- character()
  restored <- character()

  for (pkg in inconsistent_pkgs) {
    if (pkg %in% restored) next

    tryCatch({
      renv::restore(packages = pkg, transactional = FALSE, prompt = FALSE)

      # Refresh status to capture any new consistent dependencies installed
      status_tbl <- project_packages_status_tbl()
      newly_consistent <- status_tbl |>
        dplyr::filter(consistent == 1) |>
        dplyr::pull(package)

      restored <- union(restored, newly_consistent)
    }, error = function(e) {
      msg <- conditionMessage(e)
      category <- classify_error(msg)
      failed <<- c(failed, pkg)
      error_log[[pkg]] <<- list(message = msg, category = category)
    })
  }

  if (upgrade_failed && length(failed)) {
    for (pkg in failed) {
      tryCatch({
        renv::update(pkg)
      }, error = function(e) {
        # silently continue
      })
    }
  }

  message("Final restore pass (non-transactional)...")
  tryCatch({
    renv::restore(transactional = FALSE, prompt = FALSE)
  }, error = function(e) {
    message("Final restore encountered errors but continuing.")
  })

  # Final error tibble
  if (length(error_log) > 0) {
    tibble::tibble(
      package = names(error_log),
      category = sapply(error_log, function(x) x$category),
      message = sapply(error_log, function(x) x$message)
    )
  } else {
    tibble::tibble(
      package = character(),
      category = character(),
      message = character()
    )
  }
}

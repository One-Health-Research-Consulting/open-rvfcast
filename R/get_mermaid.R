get_mermaid <- function(
    targets_only = FALSE,
    names = NULL,
    shortcut = FALSE,
    allow = NULL,
    exclude = ".Random.seed",
    outdated = TRUE,
    label = NULL,
    legend = TRUE,
    color = TRUE,
    reporter = targets::tar_config_get("reporter_outdated"),
    seconds_reporter = targets::tar_config_get("seconds_reporter"),
    callr_function = callr::r,
    callr_arguments = targets::tar_callr_args_default(callr_function),
    envir = parent.frame(),
    script = targets::tar_config_get("script"),
    store = targets::tar_config_get("store")
) {
  tar_assert_allow_meta("tar_mermaid", store = store)
  force(envir)
  tar_assert_lgl(targets_only, "targets_only must be logical.")
  tar_assert_lgl(outdated, "outdated in tar_mermaid() must be logical.")
  tar_assert_in(label, c("time", "size", "branches"))
  tar_assert_lgl(legend)
  tar_assert_lgl(color)
  tar_assert_scalar(legend)
  tar_assert_scalar(color)
  tar_config_assert_reporter_outdated(reporter)
  tar_assert_callr_function(callr_function)
  tar_assert_list(callr_arguments, "callr_arguments mut be a list.")
  tar_assert_dbl(seconds_reporter)
  tar_assert_scalar(seconds_reporter)
  tar_assert_none_na(seconds_reporter)
  tar_assert_ge(seconds_reporter, 0)
  targets_arguments <- list(
    path_store = store,
    targets_only = targets_only,
    names_quosure = rlang::enquo(names),
    shortcut = shortcut,
    allow_quosure = rlang::enquo(allow),
    exclude_quosure = rlang::enquo(exclude),
    outdated = outdated,
    label = label,
    legend = legend,
    color = color,
    reporter = reporter,
    seconds_reporter = seconds_reporter
  )
  callr_outer(
    targets_function = tar_mermaid_inner,
    targets_arguments = targets_arguments,
    callr_function = callr_function,
    callr_arguments = callr_arguments,
    envir = envir,
    script = script,
    store = store,
    fun = "tar_mermaid"
  )
}
  
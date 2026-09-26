#' Define the interaction constraints used by every model fit in the pipeline
#'
#' Constraints are given by predictor name (as the predictors are named after the recipe is
#' baked, e.g. "lat", "anomaly_not_sero_360") rather than by column position, because
#' positions depend on the recipe and can shift. Names are converted to xgboost's 0-based
#' positions at fit time by fit_constrained_workflow().
#'
#'
#' @title define_interaction_constraints
#'
#' @param groups List of character vectors of predictor names. Each element is one group of
#'   features allowed to interact with each other
#' @param unlisted_features What happens to predictors not named in any group: "isolated"
#'   (xgboost's own behaviour -- they cannot interact with anything, so with only one-name
#'   groups the whole model is additive) or "free" (they are placed together in one extra
#'   group, so they can interact with each other but not with the named features)
#' @return List with groups, unlisted_features, and tag (a short digest used in cache file
#'   names so results fitted under different constraints are never mixed up)
#' @author Morgan Kain
#' @export

define_interaction_constraints <- function(groups, unlisted_features = c("isolated", "free")) {

  unlisted_features <- match.arg(unlisted_features)

  ## Every group must be a character vector of predictor names
  stopifnot(is.list(groups), length(groups) > 0,
            all(vapply(groups, is.character, logical(1))))

  list(
    groups            = groups
  , unlisted_features = unlisted_features
  , tag               = substr(digest::digest(list(groups, unlisted_features)), 1, 8)
  )

}


#' Fit a workflow with interaction constraints resolved against the predictors it will see
#'
#' Shared by every model fit in the pipeline (tuning, the calibration harvest and the final
#' fit) so all of them build the model the same way. After fitting, the booster's own feature
#' order is compared with the names the constraints were resolved against; if they differ
#' (e.g. step_zv dropped a column), the constraints are rebuilt from the fitted order and the
#' model is refit once.
#'
#' @title fit_constrained_workflow
#'
#' @param rec Unprepped recipe (make_recipe())
#' @param training Training data the workflow is fit to
#' @param params Hyperparameter set passed to make_model()
#' @param start_p,spw Passed to make_model()
#' @param interaction_constraints Output of define_interaction_constraints(), or NULL for none
#' @param predictor_names Optional predictor names (in baked column order) to resolve the
#'   constraints against. Supplying them skips a full recipe prep; if NULL they are computed
#' @return Fitted workflow
#' @author Morgan Kain
#' @export

fit_constrained_workflow <- function(rec, training, params, start_p, spw,
                                     interaction_constraints = NULL, predictor_names = NULL) {

  ## No constraints: an ordinary fit
  if (is.null(interaction_constraints)) {
    wf <- workflow() |> add_model(make_model(params, start_p, spw)) |> add_recipe(rec)
    return(fit(wf, data = training))
  }

  if (is.null(predictor_names)) predictor_names <- get_predictor_names(rec, training)

  int_con <- resolve_interaction_constraints(predictor_names, interaction_constraints, names(training))
  wf      <- workflow() |> add_model(make_model(params, start_p, spw, int_con)) |> add_recipe(rec)
  fitted  <- fit(wf, data = training)

  ## Guard against the constraint positions pointing at the wrong columns
  used_names <- variable.names(extract_fit_engine(fitted))
  if (!identical(used_names, predictor_names)) {
    int_con <- resolve_interaction_constraints(used_names, interaction_constraints, names(training))
    wf      <- workflow() |> add_model(make_model(params, start_p, spw, int_con)) |> add_recipe(rec)
    fitted  <- fit(wf, data = training)
  }

  fitted

}


#' Predictor names, in the column order the model sees them, after the recipe is prepped
#'
#' @title get_predictor_names
#'
#' @param rec Unprepped recipe (make_recipe())
#' @param training Data to prep the recipe on
#' @return Character vector of predictor names
#' @author Morgan Kain
#' @export

get_predictor_names <- function(rec, training) {
  prep(rec, training = training) |>
    bake(new_data = training[0, ]) |>
    dplyr::select(-outbreak) |>
    colnames()
}


#' Convert name-based interaction constraints to xgboost's 0-based position form
#'
#' @title resolve_interaction_constraints
#'
#' @param predictor_names Predictor names in the column order the model sees them
#' @param interaction_constraints Output of define_interaction_constraints(), or NULL
#' @param raw_names Column names of the data before the recipe. A constrained feature missing
#'   from predictor_names but present here was removed by the recipe (e.g. step_zv on a
#'   constant column), which is harmless, so it is dropped with a warning; one missing from
#'   both is a misspelling and is an error
#' @return List of integer vectors (0-based column positions), or NULL for no constraints
#' @author Morgan Kain
#' @export

resolve_interaction_constraints <- function(predictor_names, interaction_constraints, raw_names = NULL) {

  if (is.null(interaction_constraints)) return(NULL)

  groups  <- interaction_constraints$groups
  missing <- setdiff(unlist(groups), predictor_names)

  ## A misspelled feature would otherwise silently leave it unconstrained
  misspelled <- setdiff(missing, raw_names)
  if (length(misspelled)) {
    stop("Interaction-constraint features not among the model predictors: ",
         paste(misspelled, collapse = ", "))
  }

  ## Features the recipe removed cannot be split on, so they need no constraint
  if (length(missing)) {
    warning("Interaction-constraint features removed by the recipe (e.g. constant in this ",
            "training set), so left out of the constraints: ", paste(missing, collapse = ", "))
    groups <- lapply(groups, setdiff, missing)
    groups <- groups[lengths(groups) > 0]
  }

  positions <- lapply(groups, function(g) match(g, predictor_names) - 1L)

  ## "free": every unnamed predictor goes into one extra group so they can interact
  if (interaction_constraints$unlisted_features == "free") {
    rest <- setdiff(seq_along(predictor_names) - 1L, unlist(positions))
    if (length(rest)) positions <- c(positions, list(rest))
  }

  positions

}

#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param base_score
#' @param interaction_constrains
#' @return
#' @author Emma Mendelsohn
#' @export
model_specs <- function(base_score, interaction_constraints) {

 parsnip::boost_tree(
    trees = 1000,
    tree_depth = hardhat::tune(),
    min_n = hardhat::tune(),
    loss_reduction = hardhat::tune(),                   
    sample_size = hardhat::tune(), 
    mtry = hardhat::tune(),
    learn_rate = hardhat::tune()
  ) |>
    parsnip::set_engine("xgboost", 
                        objective = "binary:logistic", 
                        base_score = base_score, # set the background/intercept rate - this allows the tree to split even when the training is all negatives
                        interaction_constraints = interaction_constraints, # do not interact on area
                        monotone_constraints = monotone_constraints # enforce positive relationship for area
                        ) |> 
    parsnip::set_mode("classification")

}

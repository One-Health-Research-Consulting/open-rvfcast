#' Build a hyperparameter grid and save it for repeat use
#'
#'
#' @title build_hyperparameter_grid

#' @param tune_pars grid of parameter ranges 
#' @param grid_path path to where to save parameter grid
#' @param overwrite boolean to overwrite or generate a new hyperparameter grid
#' @param folded_data_training
#' @param splitted_data
#' @param seed
#' @return Tibble of search grid and other needs for model tuning
#' @author Morgan Kain
#' @export

build_hyperparameter_grid <- function(tune_pars, grid_path, folded_data_training, splitted_data
                                      , overwrite, seed) {
  
  ## Make the grid path 
  create_data_directory(directory_path = grid_path)
  
  #### Hyperparameter search and tuning grid --------------------------------------
  
  set.seed(seed)
  hyper_id  <- stringi::stri_rand_strings(1, length = 15, pattern = "[A-Za-z0-9]")
  grid_path <- paste(grid_path, "/hypergrid_", hyper_id, ".Rds", sep = "")
  
  ## load previously saved if available for consistency
  ## Check if saved file exists and not overwrite
  if (file.exists(grid_path) && !overwrite) {
    
    par_grid <- readRDS(grid_path)
    
  } else {
    
    par_grid <- with(tune_pars
         ## Number of alternative grid options available, but space_filling efficient
         ## NOTE: Could possibly do a bit better to save some computation time by
         ## cutting out some of the parameter space where the combination of has some
         ## combination of hyperparameters that don't make a lot of sense
         , grid_space_filling(
             trees(range          = c(tree_min, tree_max))
           , tree_depth(range     = c(tree_dep_min, tree_dep_max))
           , learn_rate(range     = c(learn_rate_min, learn_rate_max))
           , min_n(range          = c(minn_min, minn_max))
           , loss_reduction(range = c(loss_red_min, loss_red_max))
           ## Arbitrary choice here in which train_inner, doesn't matter which
           , finalize(mtry(range = c(mtry_min, unknown())), folded_data_training$inner_folds[[10]] |>
                        left_join(
                          splitted_data$train_data[[1]], by = "index") |>
                        filter(cluster != 1))
           ## Total number of combinations of hyperparameters
           , size = size)) |>
      mutate(index = seq_len(n()), .before = 1)
    
    saveRDS(par_grid, grid_path)
    
  }
  
  ## return
  tibble(
    par_grid = par_grid |> list()
  , grid_id  = hyper_id
  )
  
}

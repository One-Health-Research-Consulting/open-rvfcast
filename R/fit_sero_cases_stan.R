#' Fit the Bayesian spatio-temporal kernel model

#' @title fit_sero_cases_stan

#' @param stan_dat prepped stan data
#' @param outpath path to save fitted model
#' @param overwrite boolean if true ignores if there is a saved file and refits 
#' @return tibble of data
#' @author Morgan Kain
#' @export

fit_sero_cases_stan <- function(stan_dat, outpath, overwrite) {
  
  if (file.exists(outpath) & !overwrite) {
    print("Model already fit, returning previously saved model")
    return(outpath)
  }
    
    stan_fit <- stan(
      file    = "sero_kernel_icar_base.stan"
    , data    = stan_data
    , chains  = 4
    , iter    = 20000
    , warmup  = 5000
    , seed    = 10001
    , control = list(adapt_delta = 0.95, max_treedepth = 13)
    )
    
    saveRDS(stan_fit, outpath)
    
    return(outpath)
    
}


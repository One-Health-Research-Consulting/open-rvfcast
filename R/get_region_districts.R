#' Function to get multiple region boundaries and join them retaining both country and ADM2 region information
#'
#'
#' @title get_region_districts

#' @param countries list of all countries for which ADM2 boundary information is needed
#' @return Tibble of country and ADM2 region information
#' @author Morgan Kain
#' @export

get_region_districts <- function(countries) {

  all_boundaries <- lapply(countries %>% as.list(), FUN = function(x) {
    a <- try({rgeoboundaries::geoboundaries(x, "adm2")}, silent = T)
    if (class(a)[1] == "try-error") {
      a <- try({rgeoboundaries::geoboundaries(x, "adm1")}, silent = T)
    }
    a
  })
  
  which_errored <- which(lapply(all_boundaries, class) == "try-error")
  
  if (length(which_errored) > 0) {
    all_boundaries <- all_boundaries[-which(lapply(all_boundaries, class) == "try-error")]
  }
  
  return(all_boundaries)
  
}

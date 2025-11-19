#' Put together a report composed of all of the model evaluation figures 
#'
#'
#' @title build_report

#' @param calcurves target plotted_calibration
#' @param fitswithin target examined_fits_within
#' @param fitsacross target examined_fits_across
#' @param viacross variable importance across all fits
#' @param outpath path to where to save output
#' @param overwrite Boolean to recalculate and save over a previously saved file or not
#' @return list of figures for saving as a report
#' @author Morgan Kain
#' @export

build_report <- function(calcurves, fitswithin, fitsacross, viacross, outpath, overwrite) {
 
  if (file.exists(outpath) & !overwrite) {
    message("Report already saved")
    return(outpath)
  }

  set_of_figs <- c(
    list(
    calcurves$calplot.opt[[1]]
  , calcurves$calplot.even[[1]]
  , fitsacross$conf_mat_time[[1]]
  , fitsacross$map_pred_time[[1]]
    )
  , viacross$vi_gg
  , viacross$pdp_gg
  , fitswithin$prob_dens_plot
  , fitswithin$conf_mat_plot
  , fitswithin$map
  )
  
  print(length(set_of_figs))
  
  out_file <- outpath
  dir.create(dirname(out_file), showWarnings = FALSE)
  
  pdf(out_file, width = 7, height = 5)
  for (p in seq_along(set_of_figs)) {
    if (length(class(set_of_figs[[p]])) > 2) {
      grid::grid.newpage()
      grid::grid.draw(set_of_figs[[p]])
    } else {
      print(set_of_figs[[p]])
    }
  }
  
  dev.off()
  
  return(outpath)
  
}


#' Save individual figures for making report since loading the actual targets that house
#' the ggplots is so RAM intensive and slow
#'
#'
#' @title build_report

#' @param input target that has the figures of interest
#' @param outpath path to save the figure
#' @param idinfo target that has the date info that can be joined in
#' @param plotname name of the column that houses the plot of interest
#' @param overwrite
#' @return list of figures for saving as a report
#' @author Morgan Kain
#' @export

save_fig_pieces <- function(input, outpath, idinfo, plotname, overwrite) {

  te      <- input |> qread() |> left_join(., idinfo |> dplyr::select(outer_fold_id, assess_range))
  unifold <- unique(te$outer_fold_id)

  if (plotname != "map_split") {
   for (i in seq_along(unifold)) {

    te.t <- te |> filter(outer_fold_id == unifold[i])

    out_file <- paste(outpath, "gg_", gsub("[.]", "_", plotname), "_", te.t$outer_fold_id[1], ".svg", sep = "")

    if (!file.exists(out_file) || overwrite) {

    for (j in seq_len(nrow(te.t))) {
      te.t[j, plotname][[1]][[1]] <- te.t[j, plotname][[1]][[1]] +
        theme(
          axis.text.x = element_text(size = 8)
        , axis.text.y = element_text(size = 8)
        , axis.title.x = element_text(size = 10)
        , axis.title.y = element_text(size = 10)
        ) +
        ggtitle(te.t[j, ]$assess_range)
    }

    gg.t <- patchwork::wrap_plots(te.t |> pull(get(plotname)), ncol = 2)

    ## Using svg for quarto html
    try({
        ggsave(out_file, plot = gg.t, width = 8, height = 7)
    }, silent = TRUE)

    }

   }
    ## Far too many maps to save them all as separate figures, so compile a different
     ## pdf for each level of aggregation with a different page per date
    } else {

    uniag <- unique(te$aggregation)

    for (i in seq_along(uniag)) {

    te.t     <- te |> filter(aggregation == uniag[i])

      for (p in seq_len(nrow(te.t))) {

        out_file <- paste(
            gsub("figure_pieces/maps_wide/", "", outpath)
          , "gg_", gsub("[.]", "_", plotname)
          , "_", gsub("[ ]", "_", te.t$aggregation[1]), ".pdf", sep = "")

        if (!file.exists(out_file) || overwrite) {

          dir.create(dirname(out_file), showWarnings = FALSE)
          cairo_pdf(out_file, width = 8, height = 7)
          for (p in seq_len(nrow(te.t))) {
            print(te.t[p, ]$map_split[[1]])
          }
          dev.off()

        }

    }

    }

    ## Also save two example maps of each aggregation for the report

    for (i in seq_along(uniag)) {

      ## pick two random examples for the report
      te.t     <- te |> filter(aggregation == uniag[i], date_range %in% sample(date_range, 2))

      for (p in seq_len(nrow(te.t))) {

      out_file <- paste(
        outpath
      , "gg_", gsub("[.]", "_", plotname)
      , "_", gsub("[ ]", "_", te.t$aggregation[1])
      , "_", p
      , ".svg", sep = "")

      if (!file.exists(out_file) || overwrite) {
        try({
            ggsave(out_file, plot = te.t[p, ]$map_split[[1]], width = 8, height = 7)
        }, silent = TRUE)
      }

      }

    }

  }

}

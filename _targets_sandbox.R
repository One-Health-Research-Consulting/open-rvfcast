# Load packages (in packages.R) and load project-specific functions in R folder
suppressPackageStartupMessages(source("packages.R"))
for (f in list.files(here::here("R"), full.names = TRUE)) source (f)

imported_targets <- tar_plan(
  wahis_rvf_outbreaks_raw =  qs::qdeserialize(paws::s3()$get_object(Sys.getenv("AWS_BUCKET_ID"), Key="_targets/wahis_rvf_outbreaks_raw")$Body)
)

plot_targets <- tar_plan(
  south_africa_outbreak_scale_map = structure(make_south_africa_outbreak_scale_map(wahis_rvf_outbreaks_raw), fig.width = 10, fig.height = 10),
  africa_outbreak_scale_map =       structure(make_africa_outbreak_scale_map(wahis_rvf_outbreaks_raw), fig.width = 10, fig.height = 10),
  south_africa_outbreaks_timeline = structure(make_south_africa_outbreaks_timeline(wahis_rvf_outbreaks_raw), fig.width = 10, fig.height = 10)
)

plot_file_targets <- tar_plan(
  tar_combine(allplots, plot_targets, command = vctrs::vec_c(list(!!!.x))),
  tar_file(png_plots, ggsave(
    paste0("outputs/", names(allplots), ".png"), allplots[[1]],
    units = "in", bg = "white",
    width = attr(allplots[[1]], "fig.width"), height = attr(allplots[[1]], "fig.height")),
    pattern = map(allplots)),
  tar_file(svg_plots, ggsave(
    paste0("outputs/", names(allplots), ".svg"), allplots[[1]],
    units = "in", bg = "white",
    width = attr(allplots[[1]], "fig.width"), height = attr(allplots[[1]], "fig.height")),
    pattern = map(allplots))
)

all_targets()

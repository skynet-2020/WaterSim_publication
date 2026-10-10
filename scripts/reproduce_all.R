scripts <- c(
  "scripts/figure_1.R",
  "scripts/figure_3.R",
  "scripts/figure_4.R",
  "internal_validation/frontier_bounds/scripts/frontier_identifiability_all_thresholds.R",
  "scripts/figure_5.R",
  "scripts/figure_6.R",
  "scripts/figure_7.R"
)

if (!all(file.exists(scripts))) {
  stop("Run this script from the WaterSim_publication directory.")
}

for (script in scripts) {
  message("Running ", script)
  status <- system2(file.path(R.home("bin"), "Rscript"), script)
  if (!identical(status, 0L)) {
    stop("Reproduction failed while running ", script, ".")
  }
}

message("Finished reproducing data-derived main-text Figures 1 and 3-7.")
message("Figure 2 is retained manuscript artwork and has no R generator.")

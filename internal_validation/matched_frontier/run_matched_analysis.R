scripts <- c(
  "internal_validation/matched_frontier/scripts/frontier_matched_identifiability.R",
  "internal_validation/matched_frontier/scripts/frontier_matched_effects.R",
  "internal_validation/matched_frontier/scripts/figure_matched_frontier_contrasts.R"
)

if (!all(file.exists(scripts))) {
  stop("Run this script from the WaterSim_publication repository root.")
}

for (script in scripts) {
  message("Running ", script)
  status <- system2(file.path(R.home("bin"), "Rscript"), script)
  if (!identical(status, 0L)) {
    stop("Matched-analysis workflow failed while running ", script, ".")
  }
}

message("Matched-frontier verification completed successfully.")

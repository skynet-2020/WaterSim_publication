scripts <- c(
  "supplementary/scripts/supplementary_figure_1.R",
  "supplementary/scripts/supplementary_figure_2.R"
)

if (!all(file.exists(scripts))) {
  stop("Run this script from the WaterSim_publication repository root.")
}

for (script in scripts) {
  message("Running ", script)
  status <- system2(file.path(R.home("bin"), "Rscript"), script)
  if (!identical(status, 0L)) {
    stop("Supplementary reproduction failed while running ", script, ".")
  }
}

message("Regenerated Supplementary Figures S1 and S2.")

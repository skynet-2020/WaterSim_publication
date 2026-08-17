# Capture the software environment used to reproduce the publication figures.
# Run from the WaterSim_publication directory.

output_path <- "documentation/software_versions.txt"
if (!dir.exists("documentation")) {
  stop("Run this script from the WaterSim_publication directory.")
}

required_packages <- c(
  "cowplot",
  "data.table",
  "ggplot2",
  "ggspatial",
  "ggtext",
  "gridExtra",
  "patchwork",
  "ragg",
  "scales",
  "sf",
  "shadowtext"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop("Missing required packages: ", paste(missing_packages, collapse = ", "))
}

package_lines <- vapply(required_packages, function(package) {
  sprintf("%-12s %s", package, as.character(utils::packageVersion(package)))
}, character(1))

sf_versions <- sf::sf_extSoftVersion()
sf_lines <- sprintf("%-12s %s", names(sf_versions), unname(sf_versions))

session_lines <- capture.output(utils::sessionInfo())

lines <- c(
  "SOFTWARE VERSIONS",
  "=================",
  "",
  "This file records the environment that successfully reproduced all figures.",
  "Regenerate it by running:",
  "",
  "    Rscript scripts/capture_software_versions.R",
  "",
  "R ENVIRONMENT",
  "-------------",
  paste("R version:", R.version.string),
  paste("Platform:", R.version$platform),
  paste("Operating system:", R.version$os),
  "IDE: Not required. The scripts run with Rscript and do not depend on",
  "RStudio or Positron.",
  "",
  "DIRECTLY REQUIRED R PACKAGES",
  "----------------------------",
  package_lines,
  "",
  "SPATIAL LIBRARIES REPORTED BY sf",
  "--------------------------------",
  sf_lines,
  "",
  "FULL R SESSION INFORMATION",
  "--------------------------",
  session_lines,
  ""
)

writeLines(lines, output_path, useBytes = TRUE)
message("Saved: ", output_path)

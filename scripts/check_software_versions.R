# Compare installed packages with the versions used for verified reproduction.
verified <- c(
  cowplot = "1.1.3", data.table = "1.15.4", ggplot2 = "3.5.1",
  ggspatial = "1.1.9", ggtext = "0.1.2", gridExtra = "2.3",
  patchwork = "1.2.0", ragg = "1.3.2", scales = "1.3.0",
  sf = "1.0.19", shadowtext = "0.1.4"
)

installed <- vapply(names(verified), function(package) {
  if (!requireNamespace(package, quietly = TRUE)) return(NA_character_)
  as.character(utils::packageVersion(package))
}, character(1))

comparison <- data.frame(
  package = names(verified), verified = unname(verified),
  installed = unname(installed),
  matches = !is.na(installed) & installed == unname(verified),
  row.names = NULL
)
print(comparison, row.names = FALSE)

if (!all(comparison$matches)) {
  stop(
    "Installed packages do not exactly match the verified environment. ",
    "See documentation/software_versions.txt."
  )
}

cat("All directly required package versions match the verified environment.\n")

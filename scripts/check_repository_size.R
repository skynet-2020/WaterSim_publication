# Pre-publication file-size check. Run from the WaterSim_publication directory.
hard_limit <- 95 * 1024^2
warning_limit <- 50 * 1024^2

files <- list.files(
  ".", recursive = TRUE, all.files = TRUE,
  full.names = TRUE, include.dirs = FALSE, no.. = TRUE
)
files <- files[!grepl("^\\./?\\.git(/|$)", files)]
sizes <- file.info(files)$size

too_large <- files[sizes >= hard_limit]
if (length(too_large)) {
  stop(
    "Files at or above the 95 MiB repository safety limit:\n",
    paste(sprintf("  %.1f MiB  %s", sizes[sizes >= hard_limit] / 1024^2,
                  too_large), collapse = "\n")
  )
}

large <- files[sizes >= warning_limit]
if (length(large)) {
  warning(
    "Files at or above 50 MiB:\n",
    paste(sprintf("  %.1f MiB  %s", sizes[sizes >= warning_limit] / 1024^2,
                  large), collapse = "\n")
  )
}

largest <- order(sizes, decreasing = TRUE)[seq_len(min(10L, length(files)))]
cat("Repository size check passed. Largest files:\n")
cat(sprintf("%6.1f MiB  %s\n", sizes[largest] / 1024^2, files[largest]))

# Validate the complete public package. Run from the repository root.
suppressPackageStartupMessages(library(data.table))

required_files <- c(
  ".gitignore",
  ".gitattributes",
  "README.md",
  "LICENSE",
  "LICENSE-DATA.md",
  "configuration/tertiary_run.txt",
  "configuration/scenario_design_explained.txt",
  "configuration/verify_scenario_count.R",
  "current_manuscript/WaterSim_manuscript_10.9.2026_v4.docx",
  "current_manuscript/WaterSim_supplementary_material_10.1.2026.docx",
  "data/merged_ob_data.csv.gz",
  "data/population_growth_data.csv.gz",
  "data/spatial/ICB_provider_areas_transformed.rds",
  "data/spatial/us_states_2024_cartographic.rds",
  "data/spatial/phoenix_ama/AMA_and_INA.shp",
  "data/spatial/phoenix_ama/AMA_and_INA.shx",
  "data/spatial/phoenix_ama/AMA_and_INA.dbf",
  "data/spatial/phoenix_ama/AMA_and_INA.prj",
  "documentation/figure_source_inventory.txt",
  "documentation/figure_4_5_6_methods_and_legacy_figure_5.txt",
  "documentation/repository_cleanup_2026-10-10.txt",
  "documentation/software_versions.txt",
  "documentation/spatial_data_sources.txt",
  "documentation/variable_descriptions.txt",
  "figures/figure_1.png",
  "figures/figure_2.png",
  "figures/figure_3.png",
  "figures/figure_4.png",
  "figures/figure_5.png",
  "figures/figure_6.png",
  "figures/figure_7.png",
  "supplementary/figures/supplementary_figure_1.png",
  "supplementary/figures/supplementary_figure_2.png",
  "supplementary/figures/supplementary_figure_3.png",
  "scripts/common_data.R",
  "scripts/figure_1.R",
  "scripts/figure_1_provider_map.R",
  "scripts/figure_3.R",
  "scripts/figure_4.R",
  "scripts/figure_5.R",
  "scripts/figure_6.R",
  "scripts/figure_7.R",
  "supplementary/scripts/supplementary_figure_1.R",
  "supplementary/scripts/supplementary_figure_2.R",
  "supplementary/reproduce_supplementary.R",
  "internal_validation/frontier_bounds/scripts/frontier_identifiability_all_thresholds.R",
  "internal_validation/matched_frontier/scripts/frontier_matched_identifiability.R",
  "internal_validation/matched_frontier/scripts/frontier_matched_effects.R",
  "internal_validation/matched_frontier/scripts/figure_matched_frontier_contrasts.R",
  "internal_validation/matched_frontier/run_matched_analysis.R",
  "additional_analysis/overall_feasibility/scripts/figure_overall_feasibility.R",
  "scripts/reproduce_all.R",
  "scripts/capture_software_versions.R",
  "scripts/check_software_versions.R",
  "scripts/check_repository_size.R"
)

missing <- required_files[!file.exists(required_files)]
if (length(missing)) {
  stop("Missing required files:\n", paste("  ", missing, collapse = "\n"))
}
empty <- required_files[file.info(required_files)$size == 0]
if (length(empty)) {
  stop("Empty required files:\n", paste("  ", empty, collapse = "\n"))
}
cat("Required-file check passed.\n")

primary <- fread("data/merged_ob_data.csv.gz")
stopifnot(
  identical(names(primary), c("field", "value", "year", "scenario", "var_type")),
  nrow(primary) == 7411500L,
  uniqueN(primary$scenario) == 24300L,
  setequal(unique(primary$field),
           c("SINYRGWR", "SINDENV", "POWSTORE", "MEDSTORE", "SVRSTORE")),
  identical(range(primary$year), c(2000L, 2060L)),
  identical(unique(primary$var_type), "OB"),
  !anyNA(primary)
)
scenario_2060 <- primary[field == "SINYRGWR" & year == 2060]
stopifnot(nrow(scenario_2060) == 24300L, uniqueN(scenario_2060$scenario) == 24300L)
rm(primary, scenario_2060)
gc()
cat("Primary-data check passed: 7,411,500 rows and 24,300 scenarios.\n")

population <- fread("data/population_growth_data.csv.gz")
stopifnot(
  identical(names(population),
            c("provider", "field", "value", "year", "scenario", "var_type")),
  nrow(population) == 305976L,
  uniqueN(population$scenario) == 76L,
  uniqueN(population$provider) == 33L,
  setequal(unique(population$field), c("POPUSED", "POPOTHER")),
  identical(range(population$year), c(2000L, 2060L)),
  identical(unique(population$var_type), "OP"),
  !anyNA(population)
)
lookup <- population[field == "POPUSED" & year == 2060,
  .(regional_population = sum(value), providers = uniqueN(provider)),
  by = scenario]
lookup[, WEBPOPGR := as.integer(sub("^growth_", "", scenario))]
setorder(lookup, WEBPOPGR)
stopifnot(
  identical(lookup$WEBPOPGR, seq(0L, 150L, by = 2L)),
  all(lookup$providers == 33L),
  all(diff(lookup$regional_population) > 0)
)
rm(population, lookup)
gc()
cat("Population-data and regional-lookup checks passed.\n")

spatial_rds <- c(
  "data/spatial/ICB_provider_areas_transformed.rds",
  "data/spatial/us_states_2024_cartographic.rds"
)
stopifnot(all(vapply(spatial_rds, function(path) inherits(readRDS(path), "sf"),
                         logical(1))))
stopifnot(nrow(sf::st_read(
  "data/spatial/phoenix_ama/AMA_and_INA.shp", quiet = TRUE
)) > 0L)
cat("Spatial-data check passed.\n")

all_files <- list.files(
  ".", recursive = TRUE, all.files = TRUE, full.names = TRUE,
  include.dirs = FALSE, no.. = TRUE
)
all_files <- all_files[!grepl("^\\./?\\.git(/|$)", all_files)]
stopifnot(all(file.info(all_files)$size < 95 * 1024^2))

release_reference_docs <- grepl(
  "^\\./current_manuscript/[^/]+\\.docx$", all_files, ignore.case = TRUE
)
prohibited_extensions <- "\\.(docx?|odt|bib|ris|enw|json|py|ipynb|pdf)$"
prohibited_files <- all_files[
  grepl(prohibited_extensions, all_files, ignore.case = TRUE) &
    !release_reference_docs
]
if (length(prohibited_files)) {
  stop(
    "Prohibited release file types detected:\n",
    paste("  ", prohibited_files, collapse = "\n")
  )
}

text_files <- all_files[grepl("\\.(R|md|txt)$", all_files, ignore.case = TRUE)]
text <- unlist(lapply(text_files, readLines, warn = FALSE), use.names = FALSE)
forbidden_patterns <- c(
  paste0("/", "Users/"),
  "output_json_dict\\.json",
  "[A-Za-z0-9_-]+_(request|response)\\.json",
  paste0("WaterSim", "Run"),
  paste0("Generate", "Queries"),
  paste0("stats/", "R_scripts"),
  paste0("photos/", "updated")
)
for (pattern in forbidden_patterns) {
  if (any(grepl(pattern, text, ignore.case = TRUE, perl = TRUE))) {
    stop("Prohibited public-package text matched pattern: ", pattern)
  }
}
cat("File-size and prohibited-content checks passed.\n")
cat("Complete package validation passed.\n")

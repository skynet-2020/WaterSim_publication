config_path <- file.path("configuration", "tertiary_run.txt")

if (!file.exists(config_path)) {
  stop("Run this script from the WaterSim_publication directory.")
}

config <- trimws(readLines(config_path, warn = FALSE))

value_for <- function(variable) {
  line <- grep(paste0("^", variable, ":"), config, value = TRUE)
  if (length(line) != 1L) {
    stop("Expected one configuration entry for ", variable, ".")
  }
  trimws(sub("^[^:]+:", "", line))
}

expand_range <- function(specification) {
  values <- as.integer(strsplit(specification, ",", fixed = TRUE)[[1]])
  if (length(values) != 3L || anyNA(values)) {
    stop("Invalid range specification: ", specification)
  }
  seq(values[1], values[2], by = values[3])
}

population_levels <- expand_range(value_for("WEBPOPGR"))
climate_pairs <- strsplit(value_for("Climate"), ";", fixed = TRUE)[[1]]
municipal_efficiency_levels <- expand_range(value_for("PCRDGPCD"))
agricultural_efficiency_levels <- expand_range(value_for("WEBAGEFF"))
transfer_levels <- expand_range(value_for("WEBAGTR1"))
augmentation_levels <- expand_range(value_for("WEBAUGPCT"))
environmental_flow_levels <- expand_range(value_for("ENFLOPCT"))

factor_counts <- c(
  population_growth = length(population_levels),
  climate_condition = length(climate_pairs),
  municipal_efficiency = length(municipal_efficiency_levels),
  agricultural_efficiency = length(agricultural_efficiency_levels),
  agricultural_transfer = length(transfer_levels),
  supply_augmentation = length(augmentation_levels),
  environmental_flow = length(environmental_flow_levels)
)

scenario_count <- prod(factor_counts)

print(factor_counts)
cat("Total scenarios:", format(scenario_count, big.mark = ","), "\n")

stopifnot(scenario_count == 24300)
cat("Verification passed: the configuration produces 24,300 scenarios.\n")

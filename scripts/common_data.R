suppressPackageStartupMessages({
  library(data.table)
})

primary_data_path <- "data/merged_ob_data.csv.gz"
population_data_path <- "data/population_growth_data.csv.gz"

if (!file.exists(primary_data_path) || !file.exists(population_data_path)) {
  stop("Run the reproduction scripts from the WaterSim_publication directory.")
}

decode_scenarios <- function(x) {
  parts <- tstrsplit(x, "_", fixed = TRUE)
  data.table(
    WEBPOPGR = as.integer(sub("^W", "", parts[[1]])),
    COCLMADJ = as.integer(sub("^CA", "", parts[[2]])),
    SVCLMADJ = as.integer(sub("^CS", "", parts[[3]])),
    PCRDGPCD = as.integer(sub("^P", "", parts[[4]])),
    WEBAGEFF = as.integer(sub("^W", "", parts[[5]])),
    WEBAGTR1 = as.integer(sub("^W", "", parts[[6]])),
    WEBAUGPCT = as.integer(sub("^W", "", parts[[7]])),
    ENFLOPCT = as.integer(sub("^E", "", parts[[8]]))
  )
}

read_primary_results <- function(fields = NULL, years = NULL) {
  result <- fread(primary_data_path)
  if (!is.null(fields)) result <- result[field %in% fields]
  if (!is.null(years)) result <- result[year %in% years]
  decoded <- decode_scenarios(result$scenario)
  cbind(result, decoded)
}

read_population_lookup <- function() {
  population <- fread(population_data_path)
  lookup <- population[field == "POPUSED" & year == 2060,
    .(population = sum(value)), by = scenario]
  lookup[, WEBPOPGR := as.integer(sub("^growth_", "", scenario))]
  setorder(lookup, WEBPOPGR)
  lookup
}

population_from_webpopgr <- function(values, lookup = read_population_lookup()) {
  approx(
    x = lookup$WEBPOPGR,
    y = lookup$population,
    xout = values,
    method = "linear",
    rule = 2
  )$y
}

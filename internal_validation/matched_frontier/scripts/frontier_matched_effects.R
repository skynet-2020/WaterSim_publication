suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})
source("scripts/common_data.R")

thresholds <- 83:93
output_directory <- "internal_validation/matched_frontier/tables"
dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)

curve_variables <- c(
  "climate", "ENFLOPCT", "PCRDGPCD", "WEBAGEFF", "WEBAGTR1",
  "WEBAUGPCT"
)

results <- read_primary_results(fields = "SINYRGWR", years = 2060)
stopifnot(
  nrow(results) == 24300L,
  uniqueN(results$scenario) == 24300L
)

results[, climate := fcase(
  COCLMADJ == 100L & SVCLMADJ == 105L, "Reference river flows",
  COCLMADJ == 70L & SVCLMADJ == 85L, "Reduced river flows",
  default = NA_character_
)]
stopifnot(!anyNA(results$climate))

# Join the exact 2060 regional POPUSED value for every experimental WEBPOPGR
# treatment. The unused growth_0 lookup record is excluded.
population_lookup <- read_population_lookup()
population_lookup <- population_lookup[WEBPOPGR %in% unique(results$WEBPOPGR)]
stopifnot(
  nrow(population_lookup) == 75L,
  identical(sort(population_lookup$WEBPOPGR), seq(2L, 150L, 2L))
)
results <- population_lookup[results, on = "WEBPOPGR"]
stopifnot(nrow(results) == 24300L, !anyNA(results$population))
setorderv(results, c(curve_variables, "WEBPOPGR"))

curve_completeness <- results[, .(
  rows = .N,
  complete_population_grid = identical(WEBPOPGR, seq(2L, 150L, 2L))
), by = curve_variables]
stopifnot(
  nrow(curve_completeness) == 324L,
  all(curve_completeness$rows == 75L),
  all(curve_completeness$complete_population_grid)
)

# Calculate classified threshold frontiers for every complete climate-policy
# curve. Censored values remain NA and are prohibited from the matched analysis.
frontiers <- results[, rbindlist(lapply(thresholds, function(threshold) {
  qualifying <- which(value >= threshold)
  below_range <- length(qualifying) == 0L
  above_range <- !below_range && value[.N] >= threshold
  frontier_index <- if (below_range) NA_integer_ else qualifying[[length(qualifying)]]
  data.table(
    gsi_threshold = threshold,
    frontier_population = if (below_range || above_range) {
      NA_real_
    } else {
      as.numeric(population[frontier_index])
    },
    frontier_class = if (below_range) {
      "below_range"
    } else if (above_range) {
      "above_range"
    } else {
      "observed"
    }
  )
})), by = curve_variables]

stopifnot(
  identical(sort(unique(frontiers$gsi_threshold)), thresholds),
  nrow(frontiers) == 324L * length(thresholds),
  all(frontiers$frontier_class == "observed"),
  !anyNA(frontiers$frontier_population)
)

factor_specifications <- list(
  list(
    variable = "climate", factor = "Climate", order = 1L,
    levels = c("Reference river flows", "Reduced river flows"),
    labels = c("Reference river flows", "Reduced river flows"),
    matched_backgrounds = 162L,
    pairs = list(c(1L, 2L))
  ),
  list(
    variable = "ENFLOPCT", factor = "Environmental flow", order = 2L,
    levels = c("0", "50"), labels = c("0 AF", "80,000 AF"),
    matched_backgrounds = 162L,
    pairs = list(c(1L, 2L))
  ),
  list(
    variable = "PCRDGPCD", factor = "MWUE", order = 3L,
    levels = c("0", "29", "58"),
    labels = c("3.2% reduction", "25% reduction", "45% reduction"),
    matched_backgrounds = 108L,
    pairs = list(c(1L, 2L), c(1L, 3L), c(2L, 3L))
  ),
  list(
    variable = "WEBAGEFF", factor = "AWUE", order = 4L,
    levels = c("100", "90", "80"),
    labels = c("Current (2015)", "10% below 2015", "20% below 2015"),
    matched_backgrounds = 108L,
    pairs = list(c(1L, 2L), c(1L, 3L), c(2L, 3L))
  ),
  list(
    variable = "WEBAGTR1", factor = "AWT", order = 5L,
    levels = c("0", "50", "100"), labels = c("0%", "50%", "100%"),
    matched_backgrounds = 108L,
    pairs = list(c(1L, 2L), c(1L, 3L), c(2L, 3L))
  ),
  list(
    variable = "WEBAUGPCT", factor = "Supply augmentation", order = 6L,
    levels = c("0", "10", "20"), labels = c("0%", "10%", "20%"),
    matched_backgrounds = 108L,
    pairs = list(c(1L, 2L), c(1L, 3L), c(2L, 3L))
  )
)

calculate_factor_outputs <- function(specification) {
  focal_variable <- specification$variable
  background_variables <- setdiff(curve_variables, focal_variable)

  matched_range <- frontiers[, {
    observed_levels <- as.character(get(focal_variable))
    stopifnot(
      .N == length(specification$levels),
      setequal(observed_levels, specification$levels),
      all(frontier_class == "observed"),
      !anyNA(frontier_population)
    )
    .(
      focal_factor = specification$factor,
      factor_order = specification$order,
      matched_frontier_range = max(frontier_population) - min(frontier_population),
      number_of_levels = .N,
      number_censored_or_missing = sum(frontier_class != "observed") +
        sum(is.na(frontier_population))
    )
  }, by = c(background_variables, "gsi_threshold")]

  stopifnot(
    nrow(matched_range) == specification$matched_backgrounds * length(thresholds),
    all(matched_range[, .N, by = gsi_threshold]$N == specification$matched_backgrounds),
    all(matched_range$number_censored_or_missing == 0L)
  )

  pairwise <- frontiers[, {
    observed_levels <- as.character(get(focal_variable))
    stopifnot(
      .N == length(specification$levels),
      setequal(observed_levels, specification$levels),
      all(frontier_class == "observed"),
      !anyNA(frontier_population)
    )
    values <- setNames(frontier_population, observed_levels)
    rbindlist(lapply(specification$pairs, function(pair) {
      first_level <- specification$levels[[pair[[1]]]]
      second_level <- specification$levels[[pair[[2]]]]
      first_label <- specification$labels[[pair[[1]]]]
      second_label <- specification$labels[[pair[[2]]]]
      data.table(
        focal_factor = specification$factor,
        factor_order = specification$order,
        contrast = paste0(second_label, " - ", first_label),
        contrast_order = which(vapply(
          specification$pairs, identical, logical(1), pair
        )),
        first_level = first_label,
        second_level = second_label,
        difference = values[[second_level]] - values[[first_level]]
      )
    }))
  }, by = c(background_variables, "gsi_threshold")]

  expected_pairwise_rows <- specification$matched_backgrounds *
    length(thresholds) * length(specification$pairs)
  stopifnot(nrow(pairwise) == expected_pairwise_rows)

  list(range = matched_range, pairwise = pairwise)
}

factor_outputs <- lapply(factor_specifications, calculate_factor_outputs)
range_raw <- rbindlist(lapply(factor_outputs, `[[`, "range"), fill = TRUE)
pairwise_raw <- rbindlist(lapply(factor_outputs, `[[`, "pairwise"), fill = TRUE)

range_raw[, matched_frontier_range_millions := matched_frontier_range / 1e6]
pairwise_raw[, difference_millions := difference / 1e6]

range_summary <- range_raw[, .(
  n = .N,
  median = median(matched_frontier_range_millions),
  q25 = as.numeric(quantile(matched_frontier_range_millions, 0.25)),
  q75 = as.numeric(quantile(matched_frontier_range_millions, 0.75)),
  minimum = min(matched_frontier_range_millions),
  maximum = max(matched_frontier_range_millions)
), by = .(factor_order, focal_factor, gsi_threshold)]
setorder(range_summary, factor_order, gsi_threshold)

pairwise_summary <- pairwise_raw[, .(
  n = .N,
  median_difference = median(difference_millions),
  q25_difference = as.numeric(quantile(difference_millions, 0.25)),
  q75_difference = as.numeric(quantile(difference_millions, 0.75)),
  minimum_difference = min(difference_millions),
  maximum_difference = max(difference_millions),
  pct_positive = 100 * mean(difference_millions > 0),
  pct_negative = 100 * mean(difference_millions < 0),
  pct_zero = 100 * mean(difference_millions == 0)
), by = .(
  factor_order, focal_factor, contrast_order, contrast, first_level,
  second_level, gsi_threshold
)]
setorder(pairwise_summary, factor_order, contrast_order, gsi_threshold)

stopifnot(
  nrow(range_raw) == (162L + 162L + 4L * 108L) * length(thresholds),
  nrow(range_summary) == length(factor_specifications) * length(thresholds),
  all(range_summary[focal_factor %in% c("Climate", "Environmental flow")]$n == 162L),
  all(range_summary[!focal_factor %in% c("Climate", "Environmental flow")]$n == 108L),
  all(range_raw$number_censored_or_missing == 0L),
  all(range_raw$matched_frontier_range >= 0),
  all(abs(
    pairwise_summary$pct_positive + pairwise_summary$pct_negative +
      pairwise_summary$pct_zero - 100
  ) < 1e-10)
)

range_raw_export <- range_raw[, c(
  "focal_factor", "gsi_threshold", setdiff(
    names(range_raw),
    c(
      "focal_factor", "factor_order", "gsi_threshold",
      "matched_frontier_range_millions"
    )
  ), "matched_frontier_range_millions"
), with = FALSE]

fwrite(
  range_raw_export,
  file.path(output_directory, "matched_frontier_range_raw.csv")
)
fwrite(
  range_summary[, setdiff(names(range_summary), "factor_order"), with = FALSE],
  file.path(output_directory, "matched_frontier_range_summary.csv")
)
fwrite(
  pairwise_summary[, setdiff(
    names(pairwise_summary), c("factor_order", "contrast_order")
  ), with = FALSE],
  file.path(output_directory, "matched_frontier_pairwise_contrasts.csv")
)

# Construct the requested audit directly from known validated group dimensions.
audit <- rbindlist(lapply(factor_specifications, function(specification) {
  data.table(
    factor = specification$factor,
    matched_backgrounds = specification$matched_backgrounds,
    gsi_thresholds = length(thresholds),
    matched_range_values = specification$matched_backgrounds * length(thresholds),
    missing_or_censored = 0L
  )
}))

cat("\nMATCHED FRONTIER EFFECTS QA\n")
print(audit)
cat("\nMEDIAN MATCHED FRONTIER RANGES (MILLIONS)\n")
print(dcast(range_summary, focal_factor ~ gsi_threshold, value.var = "median"))

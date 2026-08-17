# WaterSim publication data and figure-reproduction package

This repository contains the curated data, configuration, documentation, and R
scripts needed to reproduce the figures associated with a scenario analysis of
water sustainability and population growth in the Phoenix metropolitan area.

The package reproduces the published analyses from archived simulation outputs.

Repository creation and analysis: **Joshua Gilman**

## Repository contents

```text
WaterSim_publication/
├── configuration/   Factorial design and scenario-count verification
├── data/            Compressed tabular data and spatial inputs
├── documentation/   Data dictionary, figure inventory, and version records
├── figures/         Main-text and supplementary figures
└── scripts/         Clean R reproduction scripts
```

Important files:

- `data/merged_ob_data.csv.gz`: regional outputs for 24,300 factorial scenarios
  and simulation years 2000–2060.
- `data/population_growth_data.csv.gz`: provider-level population outputs used
  to translate population-growth inputs into regional population.
- `configuration/tertiary_run.txt`: archived factorial configuration.
- `configuration/scenario_design_explained.txt`: plain-language explanation of
  the experimental design and scenario names.
- `documentation/variable_descriptions.txt`: data dictionary, exact WaterSim
  metadata descriptions, study interpretations, and population-lookup method.
- `documentation/figure_source_inventory.txt`: mapping between manuscript
  figures, data, and scripts.
- `documentation/software_versions.txt`: R, package, and spatial-library
  versions used for verified reproduction.
- `documentation/spatial_data_sources.txt`: sources, processing, limitations,
  and reuse notices for the geographic boundary files.

## Experimental design

The experiment contains 24,300 scenarios:

```text
75 population-growth levels
 × 2 paired climate conditions
 × 3 municipal water-use-efficiency levels
 × 3 agricultural water-use-efficiency levels
 × 3 agricultural-to-urban transfer levels
 × 3 supply-augmentation levels
 × 2 environmental-flow levels
 = 24,300 scenarios
```

The combinations are elements of a factorial experiment. They do not have
assigned probabilities and should not be interpreted as equally likely futures.
See `configuration/scenario_design_explained.txt` for the input levels and the
scenario-name convention.

## Population-growth lookup

`WEBPOPGR` is a percentage of forecasted population growth. It is not a count of
people and is not an annual percentage growth rate.

The regional population lookup is generated from
`data/population_growth_data.csv.gz` by:

1. selecting `POPUSED` in 2060;
2. summing population across all 33 providers for each population-growth input;
3. decoding and ordering the corresponding `WEBPOPGR` value.

The lookup contains 76 levels from 0 through 150. The main factorial experiment
uses the 75 levels from 2 through 150 in increments of 2; level 0 is retained as
a baseline. The shared implementation is in `scripts/common_data.R` and is used
by Figures 4, 5, and 7 and Supplementary Figure 1.

## Software requirements

Verified reproduction used R 4.2.1. The directly required R packages are:

```text
cowplot
data.table
ggplot2
ggspatial
ggtext
gridExtra
patchwork
ragg
scales
sf
shadowtext
```

Install missing packages in R with:

```r
install.packages(c(
  "cowplot", "data.table", "ggplot2", "ggspatial", "ggtext",
  "gridExtra", "patchwork", "ragg", "scales", "sf", "shadowtext"
))
```

The `sf` package may require system installations of GDAL, GEOS, and PROJ.
Exact versions from the verified environment are recorded in
`documentation/software_versions.txt`. RStudio and Positron are optional; all
scripts run from a terminal with `Rscript`.

## Reproducing the figures

Run commands from the repository root.

Verify the factorial scenario count:

```bash
Rscript configuration/verify_scenario_count.R
```

Validate all required files, datasets, lookup values, spatial inputs, file
sizes, and public-package content:

```bash
Rscript scripts/validate_package.R
```

Reproduce all data-generated figures:

```bash
Rscript scripts/reproduce_all.R
```

This generates Figures 1 and 3–7 plus Supplementary Figure 1 in `figures/`.

Individual scripts can also be run separately:

| Output | Script |
|---|---|
| Figure 1 | `scripts/figure_1.R` |
| Figure 3 | `scripts/figure_3.R` |
| Figure 4 | `scripts/figure_4.R` |
| Figure 5 | `scripts/figure_5.R` |
| Figure 6 | `scripts/figure_6.R` |
| Figure 7 | `scripts/figure_7.R` |
| Supplementary Figure 1 | `scripts/supplementary_figure_1.R` |

The detailed reproduction comparison is available in
`documentation/reproduction_comparison.txt`. Results of the isolated complete
package test are recorded in `documentation/package_test_report.txt`.

## Refreshing the software-version record

To record the current R environment:

```bash
Rscript scripts/capture_software_versions.R
```

This updates `documentation/software_versions.txt`.

To verify that the installed package versions match the tested environment:

```bash
Rscript scripts/check_software_versions.R
```

The version check reports every mismatch explicitly. A mismatch does not imply
that reproduction will fail, but it identifies differences from the environment
used for the verified outputs.

## Repository file-size check

The final PNG figures are intentionally version-controlled. All current files
are below GitHub's 100 MiB per-file limit and do not require Git Large File
Storage. Before committing or publishing, run:

```bash
Rscript scripts/check_repository_size.R
```

The check fails for files at or above the repository's 95 MiB safety threshold
and warns about files at or above 50 MiB. Git cannot ignore files automatically
based on size, so this check should be run before each release.

## Data-file structure

Both tabular datasets use long format.

`merged_ob_data.csv.gz` columns:

```text
field, value, year, scenario, var_type
```

`population_growth_data.csv.gz` columns:

```text
provider, field, value, year, scenario, var_type
```

Variable definitions, units, caveats, and verbatim WaterSim descriptions are
provided in `documentation/variable_descriptions.txt`.

## Notes and known limitations

- Figure 5 can emit non-fatal warnings when a violin group contains fewer than
  two observations. The script completes and retains those observations through
  the accompanying boxplot geometry.
- Exact raster appearance can vary slightly across operating systems because of
  fonts and graphics libraries. The verified substantive comparison is recorded
  in `documentation/reproduction_comparison.txt`.

## Licensing

- R code in `scripts/` and `configuration/` is licensed under the MIT License;
  see `LICENSE`.
- Original datasets, figures, configuration files, and documentation are
  licensed under CC BY 4.0; see `LICENSE-DATA.md`.
- Third-party geographic data under `data/spatial/` are excluded from that
  license and remain subject to their source-agency terms. Sources,
  transformations, and limitations are documented in
  `documentation/spatial_data_sources.txt`.

# WaterSim publication data and analysis package

This repository contains the curated data, configuration, documentation, and R
code supporting the current WaterSim manuscript on groundwater-constrained
population growth in metropolitan Phoenix.

Repository creation and analysis: **Joshua Gilman**

## Repository organization

```text
configuration/       Factorial design and scenario-count verification
current_manuscript/  Current manuscript and supplementary-material reference files
data/                Compressed model outputs and spatial inputs
documentation/       Data dictionary, figure inventory, and audit records
figures/             Final main-text figures only
scripts/             Main-text analysis and figure scripts
supplementary/       Final supplementary figures and their available scripts
internal_validation/ Internal frontier-identifiability and matched analyses
additional_analysis/ Retained analyses not currently in the manuscript
archive/             Clearly labeled superseded publication material
```

The manuscript and supplementary-material files are included as reference
copies. The numbered PNGs in `figures/` correspond to manuscript version 4 in
`current_manuscript/`.

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

These are factorial experimental combinations, not probability-weighted
futures. See `configuration/scenario_design_explained.txt` and
`configuration/tertiary_run.txt` for exact levels and scenario conventions.

## Core data

- `data/merged_ob_data.csv.gz` contains regional outputs for the 24,300
  scenarios and simulation years 2000–2060.
- `data/population_growth_data.csv.gz` contains provider-level population
  outputs used to translate the WaterSim `WEBPOPGR` input into regional
  population.
- `documentation/variable_descriptions.txt` documents variables, units, and
  interpretation.

`WEBPOPGR` is a percentage of forecasted population growth, not a population
count or annual growth rate. The lookup selects `POPUSED` in 2060, sums across
33 providers, and maps each of the 75 experimental `WEBPOPGR` levels (2–150 in
increments of 2) to regional population. The additional level 0 is retained as
a baseline in the population dataset.

## Current manuscript figures

| Output | Subject | Primary code |
|---|---|---|
| Figure 1 | Phoenix Metropolitan Area and water providers | `scripts/figure_1.R` |
| Figure 2 | Simulation workflow | Artwork retained from the manuscript |
| Figure 3 | Distribution of 2060 GSI | `scripts/figure_3.R` |
| Figure 4 | Overall population–GSI frontier | `scripts/figure_4.R` |
| Figure 5 | Median population frontiers and IQR ribbons | `scripts/figure_5.R` |
| Figure 6 | Conditional-feasibility heatmaps | `scripts/figure_6.R` |
| Figure 7 | Reservoir storage trajectories | `scripts/figure_7.R` |

The final numbered PNGs are in `figures/`. Figure 2 is retained as manuscript
artwork because no data-generating script is required for it. See
`documentation/figure_source_inventory.txt` for exact inputs and methods.

## Supplementary material

Final supplementary figures are in `supplementary/figures/`.

- Figure S1 maps the population-growth input to 2060 regional population.
- Figure S2 shows conditional Salt–Verde storage feasibility.
- Figure S3 is the experimental-design illustration embedded in the current
  supplementary document; it is retained as artwork and has no R generator.

Available scripts are in `supplementary/scripts/`.

## Internal verification analyses

`internal_validation/frontier_bounds/` documents boundary censoring and the
selection of the 79–98 GSI interval used in Figure 5.

`internal_validation/matched_frontier/` contains the matched-background
analysis retained for internal verification and potential reviewer requests.
It includes the matched-set identifiability analysis, matched frontier
contrasts over the fully matchable interval, the final contrast figure and
supporting tables, and a concise methods document. These files are not part of
the current manuscript unless explicitly added in a later revision.

The unconditioned feasibility heatmap requested for context is retained under
`additional_analysis/overall_feasibility/`.

## Legacy material

The former exact-GSI violin version of Figure 5 is preserved under
`archive/legacy_figure_5/`. It is retained for provenance but is not the current
manuscript Figure 5. Pre-v4 figure rasters are likewise separated from the
current numbered figures.

The detailed methods record for Figure 4, the former Figure 5, current Figure
5, and current Figure 6 is
`documentation/figure_4_5_6_methods_and_legacy_figure_5.txt`.

## Running the analyses

Run commands from the repository root. Verify the factorial count and package
structure with:

```bash
Rscript configuration/verify_scenario_count.R
Rscript scripts/validate_package.R
```

Regenerate the data-derived main figures with:

```bash
Rscript scripts/reproduce_all.R
```

Figure 5 requires the boundary diagnostic table. The reproduction driver runs
that diagnostic before Figure 5. Supplementary and internal analyses are run
separately so they cannot be mistaken for current main-text outputs.

```bash
Rscript supplementary/reproduce_supplementary.R
Rscript internal_validation/matched_frontier/run_matched_analysis.R
```

## Software and licensing

The verified environment and directly required R packages are recorded in
`documentation/software_versions.txt`. RStudio and Positron are optional; the
scripts run with `Rscript`. Use `scripts/check_repository_size.R` before a
release.

- R code is licensed under the MIT License (`LICENSE`).
- Original datasets, figures, configuration files, and documentation are
  licensed under CC BY 4.0 (`LICENSE-DATA.md`).
- Third-party geographic data retain their original terms; see
  `documentation/spatial_data_sources.txt`.

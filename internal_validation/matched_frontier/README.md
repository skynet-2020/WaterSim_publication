# Matched-frontier validation

This directory contains a complete matched-background analysis of shifts in
the groundwater-constrained population frontier. For each focal climate or
policy factor, the analysis compares factor levels while holding the exact
combination of all other non-population factors constant.

The workflow first determines which GSI thresholds support complete matched
sets without substituting population boundaries for censored frontiers. It
then calculates matched frontier contrasts and produces the retained figure
and supporting tables.

Run the complete workflow from the repository root with:

```bash
Rscript internal_validation/matched_frontier/run_matched_analysis.R
```

Detailed methods and a figure caption are retained in
`documentation/matched_frontier_analysis_summary.txt`. The analysis is
separate from the main figure-reproduction workflow but is maintained as a
complete scientific validation analysis.

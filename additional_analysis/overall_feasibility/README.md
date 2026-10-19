# Overall feasibility heatmap

This additional analysis applies the Figure 6 feasibility logic to the entire
factorial ensemble without fixing a climate or policy level. For every tested
2060 regional population and minimum GSI threshold, each heatmap cell reports
the percentage of all 324 climate-policy combinations whose modeled GSI meets
or exceeds the threshold.

The analysis therefore provides an unconditioned ensemble-wide reference. It
differs from Figure 6, where each panel fixes one factor level and calculates
the percentage across only the remaining factor combinations.

Run from the repository root with:

```bash
Rscript additional_analysis/overall_feasibility/scripts/figure_overall_feasibility.R
```

Output: `figures/figure_overall_feasibility.png`

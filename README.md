# Educational Twitter Community Switching Inference

Supplementary repository for the EDM '23 paper "Timing Matters: Inferring Educational Twitter Community Switching from Membership Characteristics."

## Citation

Borchers, C., Klein, L., Johnson, H., & Fischer, C. (2023). Timing Matters: Inferring Educational Twitter Community Switching from Membership Characteristics. In Proceedings of the 16th International Conference on Educational Data Mining (EDM). Bengaluru, India.

```
@inproceedings{borchers2023timing,
  title={Timing Matters: Inferring Educational Twitter Community Switching from Membership Characteristics},
  author={Borchers, Conrad and Klein, Lennart and Johnson, Hayden and Fischer, Christian},
  booktitle={EDM23: 16th International Conference on Educational Data Mining},
  year={2023}
}
```

## Folder structure

The repository follows standard practices of the `{targets}` data analysis pipelines. To learn more about the `{targets}` package, please refer to its [documentation](https://books.ropensci.org/targets/).

`run_targets.R` runs the `{targets}` start to finish. Checkpoints are specified in `_targets.R`.

In particular, the variables 

`n_interactions_for_membership = 2`

and

`exit_quantile = 0.9`

in `_targets.R` allow for an adjustment of the community member tagging assumptions to check the robustness of the results against different membership inclusion criteria.

## Data availability

To learn more about the project data set and its availability, please refer to the [Twitter Germany project page](https://chrisfi.com/twittergermany) or send an email to Conrad Borchers at [cborcher@cs.cmu.edu](mailto:cborcher@cs.cmu.edu). 

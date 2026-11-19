# Parameterization-Invariant Sequential Inspection Planning

R code, synthetic and published data, saved numerical outputs, and validation scripts for:

> Wenhan Zhang and Xiaojun Zhu. *A Parameterization-Invariant Reliability Criterion for Sequential Inspection Planning in Accelerated Life Tests*.

Repository: <https://github.com/Wenhan-Zhang-Reliabiliy/parameterization-invariant-inspection>

The project compares Bayesian reliability, unweighted Fisher-trace, and covariance-weighted Fisher-trace criteria in rolling accelerated-life-test inspection planning. It distinguishes objective invariance under exact linear transformations from numerical sensitivity in rolling posterior approximation and optimization.

This repository contains only computational materials. Manuscript source files and compiled documents are not included.

## Contents

- `paper1/R/`: R scripts for simulation, posterior computation, optimization, analysis, and validation.
- `paper1/output/`: raw replicate-level results, derived summaries, generated figures, and validation outputs.
- `work/exploratory/voltage_case/`: voltage-case results, leave-one-out summaries, and parametric-bootstrap outputs. The directory name is retained for compatibility with the R scripts; it contains reported results rather than logs.

Saved outputs are included so that the numerical checks can be run without repeating the simulations.

## Requirements

The results were generated and validated with R 4.6.2 on Windows 11 using base R packages. See `paper1/output/final_session_info.txt` for the recorded session.

## Validate the saved results

From the repository root, run:

```bash
Rscript paper1/R/validate_saved_results.R
```

This entry point checks model identities, optimization diagnostics, primary outputs, weighted-trace experiments, and the voltage application without generating manuscript files.

## Full simulation

From the repository root, run:

```bash
Rscript paper1/R/run_all.R
```

The default is 1000 matched replicates for each main comparison. Numerical-sensitivity experiments use 200 replicates, and the refined outcome-lookahead feasibility check uses 20 replicates. A full rerun takes several hours and replaces generated outputs; use a clean copy of the repository when preserving the supplied results.

The six weighted-trace experiments use a bounded pool of four R workers by default. Set `PAPER1_WORKERS` to a value from 1 to 8 to adjust the pool. The voltage bootstrap also uses this worker setting.

The implementation supports checkpoint files. Scenario, prior-strength, grid, draw-count, and coding settings are controlled by environment variables in `run_all.R` and the individual `run_*.R` entry scripts.

## Main numerical studies

- Eight settings compare one-step and full-horizon reliability and trace policies using 1000 matched replicates.
- Two factorial experiments separate posterior averaging from criterion choice.
- Equivalent stress codings use scales 1, 2, and 10 with matched physical data and transformed priors.
- Six covariance-weighted trace series provide 6000 additional matched replicates across two scenarios and three codings.
- Saved-schedule analyses compare first-detection times and detection-stage agreement using identical physical failure times.
- Common-posterior fixed-pilot experiments evaluate bidirectional cross-coding regret.
- Rolling and common-posterior diagnostics vary grid resolution, posterior draws, and optimization starts.
- A voltage ALT illustration compares all three rolling criteria under two equivalent codings, with eight-fold leave-one-out and 200-replicate parametric-bootstrap checks.

## Voltage computational details

- The empirical prior uses independent normal distributions. Under the normalized coding, the means for `(alpha, beta, log sigma)` are `(7.711033, -0.776703, -0.904535)` with standard deviations `(0.7, 0.7, 0.4)`. Under the log-ratio coding, the means are `(7.711033, -1.915586, -0.904535)` and standard deviations are `(0.7, 1.726412, 0.4)`.
- Posterior grids contain `21^3 = 9261` points. Rolling planning uses 40 draws, and final target estimates and predictive scores use 10,000 posterior draws.
- Each optimization adds 30 random starts to the default and, when available, warm start, giving 31 or 32 starts in total.
- The historical empirical prior remains fixed during the bootstrap. A separate fit to all 24 observations supplies only the data-generating anchor.
- Leave-one-out prediction targets the held-out exact failure time. The log score is the log of the Monte Carlo average Weibull predictive density at that time.

## Output organization

Raw replicate-level files include `grid_scenario_*.csv`, `grid3d_scenario_*.csv`, `trace*_scenario_*.csv`, `factorial_scenario_*.csv`, `parameterization_scale_*.csv`, `weighted_trace_scale_*.csv`, `prior_*_scenario_*.csv`, and the sensitivity and common-regret series.

Derived files include `criterion_summary.csv`, `criterion_reductions.csv`, `revision_paired_reductions.csv`, `common_regret_summary.csv`, `numerical_sensitivity_summary.csv`, `weighted_trace_summary.csv`, and `engineering_detection_summary.csv`.

The current-prior and appendix intervals for shared comparisons use the canonical `criterion_reductions.csv` output rather than independent bootstrap runs. Diagnostic subsamples are selected using `replicate <= 200`.

Voltage-case CSV files are organized under `work/exploratory/voltage_case/`. All current result files carry the model-version identifier `weibull-cloglog-z0-v2`, and random seeds are fixed in the corresponding R scripts.

## Validation

The checks cover:

- Weibull survival, reliability-target, and finite-difference gradient identities;
- replicate counts, finite errors, convergence indicators, and model-version consistency;
- independent RMSE and Monte Carlo standard-error calculations from raw errors;
- exact agreement between current-prior and corresponding main results;
- optimizer and grid diagnostics;
- weighted-trace invariance and all 6000 weighted-trace replicates;
- voltage posterior construction, coding transformations, leave-one-out results, and bootstrap completeness.

Validation outputs are recorded in `paper1/output/` and `work/exploratory/voltage_case/validation.txt`.

## Data availability

Simulation data are generated by the R scripts. The voltage illustration uses 24 published capacitor failure times from Guangbin Yang's *Life Cycle Reliability Engineering* (2007), Example 7.3, p. 260, DOI: [10.1002/9780470117880](https://doi.org/10.1002/9780470117880). The data are embedded in `paper1/R/voltage_empirical_model.R`; no external download is required. All three joint-simulation policies end at 1000 h in every matched run; the median actual duration is 1000 h with zero interquartile range.

## Contact

Corresponding author: Xiaojun Zhu, Xi'an Jiaotong-Liverpool University.

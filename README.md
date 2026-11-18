# Parameterization-Invariant Reliability Criterion for Sequential Inspection Planning

This repository contains the R code, saved numerical outputs, and manuscript source for:

> Wenhan Zhang and Xiaojun Zhu. *A Parameterization-Invariant Reliability Criterion for Sequential Inspection Planning in Accelerated Life Tests*. Manuscript prepared in the Elsevier `cas-sc` format.

The paper embeds a Bayesian scalar-target reliability criterion in RITO-style rolling inspection planning. It compares this criterion with unweighted and covariance-weighted Fisher-trace criteria under equivalent linear model parameterizations, and evaluates both inspection schedules and first-detection outcomes. The study distinguishes objective invariance under exact transformations from numerical sensitivity in rolling posterior approximation and optimization.

## Repository contents

- `paper1/R/`: 69 R scripts for simulation, posterior computation, optimization, table generation, figures, and validation.
- `paper1/output/`: raw replicate-level results, derived summaries, generated figures, model checks, and session information.
- `work/exploratory/voltage_case/`: voltage-case results, leave-one-out summaries, and 200-replicate parametric-bootstrap outputs.
- `paper1/manuscript/`: manuscript LaTeX source, bibliography, generated table inputs, and compiled PDF.

The saved outputs are included so that the manuscript can be rebuilt without repeating the simulations. No exploratory logs, superseded figures, or unrelated case studies are included.

## Requirements

The results were generated and validated with:

- R 4.6.2 on Windows 11;
- base R packages;
- TeX Live 2026 with `cas-sc`, `elsarticle-num`, and standard LaTeX packages used in `manuscript.tex`.

See `paper1/output/final_session_info.txt` for the recorded R session.

## Rebuild from the saved outputs

Run the following command from the repository root:

```bash
Rscript paper1/R/rebuild_manuscript.R
```

This entry point regenerates summaries, figures, LaTeX tables, and result macros, then runs model, output, optimizer, cross-table, and manuscript consistency checks. It does not repeat the replicate-level simulations. On the development computer, reconstruction takes approximately one minute.

To rebuild the PDF after reconstruction:

```bash
cd paper1/manuscript
pdflatex -interaction=nonstopmode -halt-on-error manuscript.tex
bibtex manuscript
pdflatex -interaction=nonstopmode -halt-on-error manuscript.tex
pdflatex -interaction=nonstopmode -halt-on-error manuscript.tex
```

The compiled manuscript is `paper1/manuscript/manuscript.pdf`.

## Full simulation

From the repository root, run:

```bash
Rscript paper1/R/run_all.R
```

The default is 1000 matched replicates for each main comparison. Numerical-sensitivity experiments use the first 200 replicates, and the refined outcome-lookahead feasibility check uses 20 replicates. A full rerun takes several hours and replaces generated outputs; use a clean copy of the repository when preserving the supplied results. The six weighted-trace experiments use a bounded pool of four R workers by default; `PAPER1_WORKERS` can reduce this number.

The implementation supports checkpoint files. Scenario, prior-strength, grid, draw-count, and coding settings are controlled by the environment variables set in `run_all.R` and the individual `run_*.R` entry scripts.

## Main numerical studies

- Eight settings compare one-step and full-horizon reliability and trace policies using 1000 matched replicates.
- Two factorial experiments separate posterior averaging from criterion choice.
- Equivalent stress codings use scales 1, 2, and 10 with matched physical data and transformed priors.
- Six covariance-weighted trace series provide 6000 additional matched replicates across two scenarios and three codings.
- Saved-schedule analyses compare first-detection times and detection-stage agreement using identical physical failure times.
- Common-posterior fixed-pilot experiments evaluate bidirectional cross-coding regret.
- Rolling and common-posterior diagnostics vary grid resolution, posterior draws, and optimization starts.
- A voltage ALT illustration compares all three rolling criteria under two equivalent codings, with eight-fold leave-one-out and 200-replicate parametric-bootstrap checks.

## Output organization

Raw replicate-level files include `grid_scenario_*.csv`, `grid3d_scenario_*.csv`, `trace*_scenario_*.csv`, `factorial_scenario_*.csv`, `parameterization_scale_*.csv`, `weighted_trace_scale_*.csv`, `prior_*_scenario_*.csv`, and the sensitivity and common-regret series. `engineering_detection_results.csv` contains the matched first-detection outcomes. Voltage-case CSV files are organized under `work/exploratory/voltage_case/`; the directory name is retained for compatibility with the R scripts, but the included files are the reported results rather than exploratory logs.

Derived files include `criterion_summary.csv`, `criterion_reductions.csv`, `revision_paired_reductions.csv`, `common_regret_summary.csv`, `numerical_sensitivity_summary.csv`, `weighted_trace_summary.csv`, and `engineering_detection_summary.csv`. The current-prior and appendix intervals for shared comparisons are copied from the canonical `criterion_reductions.csv` output rather than bootstrapped independently.

Diagnostic subsamples are selected using `replicate <= 200`. The default scale-1 sensitivity rows therefore reproduce the first 200 rows of the corresponding main simulations; their RMSE values should not be compared directly with full-sample RMSE values.

All current result files carry the model-version identifier `weibull-cloglog-z0-v2`. Random seeds are fixed in the corresponding R scripts.

## Validation

`rebuild_manuscript.R` calls the validation scripts automatically. Important checks include:

- Weibull survival, reliability-target, and finite-difference gradient identities;
- replicate counts, finite errors, convergence indicators, and model-version consistency;
- independent RMSE and Monte Carlo standard-error calculations from raw errors;
- exact agreement between current-prior and corresponding main results;
- canonical paired bootstrap intervals across the main text and appendix;
- first-200-replicate sensitivity results against the main simulations;
- weighted-trace invariance, all 6000 new replicates, and identical physical failure times across policies and codings;
- manuscript numbers, table inputs, figure files, citations, references, and LaTeX log errors.

The validation summary is recorded in `paper1/output/validation_summary.txt`, `paper1/output/second_round_consistency.txt`, `paper1/output/ress_enhancement_validation.txt`, `paper1/output/ress_review_validation.txt`, and `paper1/output/manuscript_validation.txt`.

## Data availability

The simulation data are generated by the R scripts. The voltage illustration uses 24 published capacitor failure times, embedded in `paper1/R/voltage_empirical_model.R` with the source cited in the manuscript. No external data download is required.

## Manuscript status and contact

This is a submission-oriented manuscript, not a published article. Please check the manuscript and repository version before citing or reusing the code. Corresponding author: Xiaojun Zhu, Xi'an Jiaotong-Liverpool University.

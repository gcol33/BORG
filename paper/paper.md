---
title: "BORG: Diagnosing dependency structure and enforcing leakage-safe cross-validation in R"
tags:
  - R
  - cross-validation
  - spatial autocorrelation
  - model evaluation
  - data leakage
  - machine learning
authors:
  - name: Gilles Colling
    orcid: 0000-0003-3070-6066
    affiliation: 1
affiliations:
  - name: Department of Botany and Biodiversity Research, University of Vienna, Austria
    index: 1
date: 6 July 2026
bibliography: paper.bib
---

# Summary

Cross-validation (CV) estimates how well a model predicts unseen data, but the
estimate is only honest when the held-out data are independent of the training
data. Ecological, environmental, and clinical data rarely satisfy that
assumption: nearby locations, adjacent times, and repeated measurements on the
same unit carry shared information. Random k-fold CV then places correlated
observations in different folds and reports an error far below what the model
achieves in the field [@roberts2017].

`BORG` is an R package that tests for this structure before it splits the data.
Given a table and the columns that carry structure (coordinates, a timestamp, a
grouping identifier), `BORG` runs formal dependency tests, classifies the
severity, estimates how much random CV would understate the error, and builds a
CV scheme that holds the structure out. When a dependency is present, random
folds are disabled unless the user overrides explicitly, and the override is
logged. `BORG` also audits full evaluation pipelines for preprocessing, target,
and identifier leakage, and writes the diagnosis as a methods paragraph or a
machine-readable record for an audit trail.

# Statement of need

A defensible spatial or temporal evaluation in R today requires the analyst to
already know the data are dependent, choose a blocking scheme, set a block size,
and decide to use it. The tooling assumes that decision has been made:
`blockCV` [@blockCV], `CAST` [@CAST], `spatialsample` [@spatialsample],
`sperrorest` [@sperrorest], and `mlr3spatiotempcv` [@spatiotempcv] generate
blocked folds but leave the diagnosis and the choice to the user. Nothing tests
the data and refuses an invalid split by default. Leakage through preprocessing
or outcome-derived features is a separate and widespread failure mode
[@kaufman2012; @kapoor2023] with no dedicated R tooling at all.

The consequence is a literature of optimistic performance numbers. `BORG`
targets the gap between "a blocking scheme is available" and "the reported
number is honest for the intended prediction task." It makes the diagnosis the
default step, quantifies the risk, enforces a matched scheme, and separately
guards the preprocessing pipeline. The intended users are ecologists, spatial
epidemiologists, and applied machine-learning practitioners who need an
evaluation they can defend to a reviewer.

# State of the field

The R ecosystem covers fold *generation* well. `blockCV` provides spatial and
environmental blocking and a separate routine to estimate the autocorrelation
range; `CAST` contributes nearest-neighbour distance matching (kNNDM), which
adapts folds to the geometry of the prediction task [@knndm] and the area of
applicability [@aoa]; `spatialsample` supplies block and cluster resampling in
the tidymodels idiom; `sperrorest` and `mlr3spatiotempcv` provide spatial
partitioning for their respective frameworks. `BORG` reuses this well-developed
vocabulary of schemes and adds the layer above it: a diagnosis that decides
*whether* and *which* blocking is warranted, an inflation estimate, an
enforcement policy, and a pipeline-leakage audit.

| Capability | BORG | blockCV | CAST | others$^\dagger$ |
|:-----------------------------|:----:|:-------:|:----:|:----:|
| Diagnoses the dependence | yes | partial | partial | no |
| Estimates CV inflation | yes | no | no | no |
| Blocks random CV by default | yes | no | no | no |
| Audits pipeline leakage | yes | no | no | no |
| Writes methods text | yes | no | no | no |

: Where BORG differs from the R packages that generate the same folds.
$^\dagger$`spatialsample`, `sperrorest`, and `mlr3spatiotempcv` are fold
generators with no dependency diagnosis (all "no"). "partial" marks a related
diagnostic (`blockCV`'s autocorrelation-range estimate; `CAST`'s kNNDM geometry
matching) offered without testing for dependence or changing the default split.
Fold generation itself, which every package supports, is omitted.

# Software design

`BORG` separates diagnosis from enforcement. `borg_diagnose()` tests each supplied
structure: spatial autocorrelation via Moran's I [@moran1950], temporal
autocorrelation via the autocorrelation function and the Ljung-Box test
[@ljungbox1978], and grouped structure via the intraclass correlation. It
returns a dependency type, a severity, an estimated inflation, and a recommended
scheme. `borg_cv()` then generates the scheme; `borg()` wraps both and returns
folds as a plain list or as `rsample`, `caret`, or `mlr3` objects.

The problem the diagnosis formalizes: for a model $\hat f$ trained on
$\{(s_i, x_i, y_i)\}$ and applied at a new location $s^*$, the quantity CV
should estimate is the generalization error

$$
\mathrm{Err} = \mathbb{E}\!\left[\, L\big(y^*, \hat f(x^*)\big) \,\right],
$$

where $L$ is the loss and $s^*$ is drawn from the prediction domain. A CV
partition into training and validation sets $\{(\mathcal{T}_k, \mathcal{V}_k)\}$
estimates $\mathrm{Err}$ without bias only when the validation points are
independent of the training points. Under a positive dependency,
$\mathrm{Cov}(y_i, y_j) = C(\lVert s_i - s_j \rVert) > 0$ for close $s_i, s_j$, a
random assignment scatters correlated points across folds, so validation points
sit next to training points and $\widehat{\mathrm{Err}}_{\text{CV}} <
\mathrm{Err}$. `BORG` tests whether $C(\cdot) > 0$ and, when it is, blocks folds
so that validation points lie beyond the estimated dependence range.

A second axis guards the pipeline rather than the folds. `borg_inspect()`,
`borg_validate()`, and `borg_audit()` classify evaluation risks as
`hard_violation` (preprocessing fit on all data, train-test identifier overlap,
outcome-derived features) or `soft_inflation` (insufficient block size, post-hoc
subgroup analysis), enforcing the former and flagging the latter. `borg_aoa()`
flags prediction locations outside the training feature space [@aoa].

# Benchmark

Two experiments quantify the cost of ignoring dependence, reproducible from
`paper/bench_simulation.R` and `paper/bench_spatial_cv.R`. Both use a random
forest [@ranger], single threaded, with medians over 20 seeds.

The primary experiment supplies the ground truth that real data cannot
(\autoref{fig:bench}a). A Gaussian random field is sampled in six spatial
clusters, the common ecological sampling pattern, and an independent uniform
test set gives the true generalization error for mapping to new locations (RMSE
0.68). Random 5-fold CV reports 0.39, understating that error by 42%; `BORG`'s
spatial-block CV reports 0.60, cutting the shortfall to 13%. `BORG` diagnosed
the dependency in all 20 simulations. No scheme fully recovers the extrapolation
error under heavy clustering, but the naive estimate is optimistic by a margin
that would reverse most model-selection decisions.

A real-data check on the Meuse dataset [@sp] shows the same mechanism without
ground truth (\autoref{fig:bench}b). Predicting log zinc from distance-to-river,
elevation, organic matter, flood frequency, and soil class, the six 5-fold
schemes report RMSE from 0.33 to 0.40, a 22% spread from the CV scheme alone.
`BORG` diagnoses the dependency automatically (Moran's I $= 0.124$,
$p < 10^{-30}$, moderate severity) and recommends spatial blocking; `CAST`'s
kNNDM returns near-random folds here, correctly, because the Meuse survey samples
its domain densely enough that random CV is defensible. Which end of that 22%
range to trust is what the diagnosis decides and the other packages leave to the
user.

![Cross-validation error under different schemes. **(a)** Simulation with known
ground truth (green line = median true error, band its interquartile range).
Each dot is one of 20 seeds and the bar marks the median CV estimate: random
5-fold CV falls 42% below the true error, `BORG` spatial-block CV 13%. **(b)**
The same six schemes on the Meuse dataset: with identical data and model, the
CV scheme alone swings the reported RMSE by about 22% (0.33 to 0.40). The
orange and blue shading marks direction only, optimistic (lower) through
conservative (higher), since real data has no ground truth to rank the schemes;
the diagnosis is what decides which end to trust. Points are medians, bars the
interquartile range over 20 seeds.\label{fig:bench}](figures/benchmark.png){width=100%}

# Availability

`BORG` is on CRAN (`install.packages("BORG")`), developed at
<https://github.com/gcol33/BORG> under the MIT license, with a `testthat` suite,
continuous integration on Linux, macOS, and Windows, and documentation at
<https://gillescolling.com/BORG/>.

<!-- The AI usage disclosure below is a required JOSS section and is excluded
from the 750-1750 word target. -->

# AI usage disclosure

The package was developed using a modern AI-assisted developer stack. The author
designed the package architecture, made the algorithm-selection choices, and
wrote the implementation in a TUI-first command-line workflow, using a
self-hosted Qwen3-Coder-Next-REAP-48B-A3B model (mixture-of-experts coder, ~3B
active per token, 4-bit MLX quantization, running on a single Apple M4 Pro) for
code completion, refactoring, and boilerplate generation through Anthropic's
Claude Code CLI, with requests routed to the local model and no cloud inference.

# Acknowledgements

No external funding supported this work.

# References

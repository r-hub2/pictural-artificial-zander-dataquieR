
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `dataquieR`

<!-- badges: start -->

[![minimal R
version](https://img.shields.io/badge/R%3E%3D-4.1.0-6666ff.svg)](https://cran.r-project.org/)
[![Pipeline
Status](https://gitlab.com/libreumg/dataquier/badges/master/pipeline.svg?ignore_skipped=true)](https://libreumg.gitlab.io/dataquier/)
[![Coverage](https://codecov.io/gl/libreumg/dataquier/branch/master/graph/badge.svg?token=79TK6GQTMG)](https://app.codecov.io/gl/libreumg/dataquier)
[![CRAN-Version](https://www.r-pkg.org/badges/version/dataquieR)](https://cran.r-project.org/package=dataquieR)
![Latest
Release](https://gitlab.com/libreumg/dataquier/-/badges/release.svg?value_width=100)
[![DOI](https://img.shields.io/badge/DOI-10.32614%2FCRAN.package.dataquieR-00be00.svg)](https://doi.org/10.32614/CRAN.package.dataquieR)
[![CRAN-Downloads](https://cranlogs.r-pkg.org/badges/dataquieR)](https://www.r-pkg.org/pkg/dataquieR)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![`Lifecycle`](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![license](https://img.shields.io/badge/license-BSD_2_clause%20+%20file%20LICENSE-00be00.svg)](https://opensource.org/license/bsd-2-clause)
[![DOI](https://joss.theoj.org/papers/10.21105/joss.03093/status.svg)](https://doi.org/10.21105/joss.03093)
[![DOI](https://joss.theoj.org/papers/10.21105/joss.06581/status.svg)](https://doi.org/10.21105/joss.06581)

<!-- badges: end -->

The goal of `dataquieR` is to provide functions for assessing data
quality issues in studies, that can be used alone or in a data quality
pipeline. `dataquieR` also implements one generic pipeline producing
`htmltools` based HTML5 reports.

See also

[`https://dataquality.qihs.uni-greifswald.de`](https://dataquality.qihs.uni-greifswald.de)

------------------------------------------------------------------------

## Installation

You can install the released version of `dataquieR` from
[CRAN](https://CRAN.R-project.org/package=dataquieR) with:

``` r
install.packages("dataquieR")
```

The suggested packages can be directly installed by:

``` r
install.packages("dataquieR", dependencies = TRUE)
```

The developer version from
[`GitLab.com`](https://gitlab.com/libreumg/dataquier) can be installed
using:

``` r
if (!requireNamespace("remotes")) {
  install.packages("remotes")
}
remotes::install_gitlab("libreumg/dataquier")
```

For examples and additional documentation, please refer to our
[website](https://dataquality.qihs.uni-greifswald.de).

## Developer workflow

Internal development happens in the upstream
`libreumg/internal/QualityIndicatorFunctions` project. Use branches
named `issueXXX_short_description`, for example
`issue810_selenium_dt_dt2_reports`. The internal project protects the
`issue*_*` branch pattern; protected runners and protected CI variables
are only available on correctly named issue branches.

## dataquieR usage questionnaire

To help us improve `dataquieR`, we invite you to provide your feedback
by completing this short survey
([English](https://t1p.de/dataquieRsurvey_en) or
[German](https://t1p.de/dataquieRsurvey_de) version).

## Suggested packages

`dataquieR` reports can now use
[`plotly`](https://cran.r-project.org/package=plotly) if installed. That
means that, in the final report, you can zoom in the figures and get
information by hovering on the points, etc. To install `plotly` type:

``` r
install.packages("plotly")
```

To install all suggested packages, run:

``` r
prep_check_for_dataquieR_updates()
```

This command can also check for new beta releases of `dataquieR` from
our own server, so not from `CRAN`:

``` r
prep_check_for_dataquieR_updates(beta = TRUE)
```

By default, `dataquieR` is kept small for CRAN installation. The
convenience update helper installs `dataquieR` from source with
byte-compiled R code:

``` r
prep_check_for_dataquieR_updates()
```

If you want to keep the installed package smaller by installing without
byte compilation, use:

``` r
prep_check_for_dataquieR_updates(byte_compile = FALSE)
```

***Hint*** If you are running `dataquieR` in an un-trusted setting,
namely, inside a server application, please consider disabling the
import of R-serialization files to prevent users from importing `RData`
(or `RDS` or even `R`) files, that trigger code execution on your
machine, see, e.g., [Ivan Krylov’s
blog](https://aitap.github.io/2024/05/02/unserialize.html) for the
reason:

``` r
# prevent rio from reading potentially code-containing files
options(rio.import.trust = FALSE)
```

If you do so, the example data won’t be loaded any more.

If you are using a version \>= 2.0.0 of `rio`, this will be the default,
so for running our examples, then, you’ll have to trust our files by
using e.g.
`withr::with_options(list(rio.import.trust = FALSE), prep_get_data_frame("study_data"))`
for loading our example study data into the data-frame cache, initially
and trusting our files loaded from

- <https://dataquality.qihs.uni-greifswald.de/extdata/study_data.RData>
- <https://dataquality.qihs.uni-greifswald.de/extdata/meta_data.RData>
- <https://dataquality.qihs.uni-greifswald.de/extdata/ship_meta.RDS>
- <https://dataquality.qihs.uni-greifswald.de/extdata/ship_subset1.RDS>
- <https://dataquality.qihs.uni-greifswald.de/extdata/ship_subset2.RDS>
- <https://dataquality.qihs.uni-greifswald.de/extdata/ship_subset3.RDS>
- <https://dataquality.qihs.uni-greifswald.de/extdata/ship.RDS>

## Cluster use

`dq_report2()` and friends can compute indicator calls in parallel. The
`cores` argument accepts:

- an integer – the number of workers for a local `PSOCK` cluster spawned
  by `dataquieR` itself,
- a named list of arguments forwarded to the internal backend
  (`util_parallel_start`),
- a pre-built `parallel` cluster object of any type (`PSOCK`, `FORK`,
  `MPI` via `Rmpi`, …).

If you have already registered a cluster via
`parallel::setDefaultCluster()`, `dataquieR` reuses it instead of
spawning a new one and leaves its lifecycle to you.

### Local multicore (default)

For local multicore work the default (`PSOCK` socket cluster) is usually
fine:

``` r
dq_report2(study_data, meta_data, ..., cores = 4)
```

### `MPI` clusters (`Rmpi` / `snow`)

Any cluster created through `parallel::makeCluster(..., type = "MPI")`
(or via the [`snow`](https://cran.r-project.org/package=snow) /
[`Rmpi`](https://cran.r-project.org/package=Rmpi) chain) plugs in like
any other `parallel` cluster. Either pass it as `cores =`:

``` r
library(Rmpi)
cl <- parallel::makeCluster(mpi.universe.size() - 1L, type = "MPI")
withr::defer(parallel::stopCluster(cl))

dq_report2(study_data, meta_data, ..., cores = cl)
```

or register it as the default cluster up front and pass `cores = NULL`
to `dq_report2()` so that it uses the registered cluster:

``` r
cl <- parallel::makeCluster(8, type = "MPI")
parallel::setDefaultCluster(cl)
withr::defer({ parallel::setDefaultCluster(NULL); parallel::stopCluster(cl) })

dq_report2(study_data, meta_data, ..., cores = NULL)
```

The same pattern works for any other `makeCluster()`-style backend
(`PSOCK` to remote hosts, `FORK` on Unix, etc.).

### `HPC` schedulers (`SLURM`, `SGE`, `Torque`/`PBS`, `LSF`, …)

For batch schedulers, use `mode = "futures"` together with a
[`future`](https://cran.r-project.org/package=future) plan from
[`future.batchtools`](https://cran.r-project.org/package=future.batchtools):

``` r
install.packages(c("future", "future.batchtools"))
library(future)
library(future.batchtools)

plan(batchtools_slurm, template = "slurm.tmpl")   # or batchtools_sge / _torque / _lsf

dq_report2(study_data, meta_data, ..., mode = "futures")
```

Earlier versions reached the same schedulers through
`options(parallelMap.mode = "BatchJobs")` (or `"batchtools"`). That
dispatch path is no longer wired up; the labels are still accepted by
the new backend for source-level compatibility but degrade to sequential
execution. `mode = "futures"` is the supported replacement.

## References

- [Software Paper](https://doi.org/10.21105/joss.06581) [![JOSS
  Article](https://joss.theoj.org/papers/10.21105/joss.06581/status.svg)](https://doi.org/10.21105/joss.06581)
- [Software Paper](https://doi.org/10.21105/joss.03093) [![JOSS
  Article](https://joss.theoj.org/papers/10.21105/joss.03093/status.svg)](https://doi.org/10.21105/joss.03093)
- [Data Quality Concept
  Paper](https://doi.org/10.1186/s12874-021-01252-7)
- [Data Quality Concept and Software Web
  Site](https://dataquality.qihs.uni-greifswald.de)

## Funding – see also [here](https://dataquality.qihs.uni-greifswald.de/Contact.html)

- German Research Foundation (`https://www.dfg.de/`) (DFG:
  `SCHM 2744/3–1` – initial concept and dataquieR development,
  `SCHM 2744/9-1` – `NFDI` Task Force `COVID-19` use case application;
  `SCHM 2744/3-4` – concept extensions, ongoing )

- [European Union’s Horizon 2020 research and innovation
  program](https://research-and-innovation.ec.europa.eu/funding/funding-opportunities/funding-programmes-and-open-calls/horizon-2020_en):
  [euCanSHare, grant agreement No. 825903](http://www.eucanshare.eu/) –
  [dataquieR](https://cran.r-project.org/package=dataquieR) refinements
  and implementations in the
  [Square2](https://doi.org/10.3233/978-1-61499-753-5-549) web application.

- [National Research Data Infrastructure for Personal Health
  Data](https://www.nfdi4health.de/): `NFDI 13/1` – extension based on
  revised metadata concept, ongoing.

- German National Cohort (NAKO Gesundheitsstudie) NAKO
  (`https://nako.de/`): `BMBF` (`https://www.bmbf.de/`): `01ER1301A` and
  `01ER1801A`

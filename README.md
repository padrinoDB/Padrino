
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

## Padrino <a href='https://levisc8.github.io/Padrino'><img src='metadata/pdb_logo.png' align="right" height="139" /></a>

This repo hosts the code used to generate and upload data to the
`Padrino` data base. This is more for back end management and to help
Padriños to find their way as they get started. I’ve created a separate
repo called `RPadrino` that will be eventually become the equivalent of
`Rcompadre`. That is where prospective users should go to find an
interface to this database that is actually useful.

Currently, the data base is a set of 12 text files that consist of ASCII
representation of model formulae, the coefficients associated with said
models, and the upper/lower bounds of the state variables. We are
closing in on a finalized design, at which point it seems likely that
this will transition to an SQL instance hosted remotely. Eventually, it
would be nice to be able to generate `.rda` objects (a la Compadre)
and/or query it directly from R through an API.

## Useful Numbers

Current number of unique species, unique publications, and unique
`ipm_id`s that are in PADRINO, and have been quality checked for
accuracy. Quality checked means that for deterministic models, the
asymptotic per-capita growth rate (*λ*) is within  ± 0.03 of the
published point estimate value. For stochastic models, we check for
approximately the same stochastic population growth rate
(*λ*<sub>*s*</sub>), but do not try to replicate the analysis, as this
usually requires too many computing resources to be feasible.

| \# of Species | \# of Publications | \# of IPM id’s |
|--------------:|-------------------:|---------------:|
|            44 |                 33 |            257 |

## For developers

For now, all commits will go to the `main` branch as this is still so
early in development that keeping a separate `devel` branch is
pointless. This will change immediately before/after the first major
release.

Development of the package `RPadrino` is taking place over
[here](https://github.com/levisc8/RPadrino).

## For Compadrinos

The digitization guide and other help files are located in
`metadata/digitization` and on the project’s
[webpage](https://levisc8.github.io/Padrino/). The
[pdbDigitUtils](https://github.com/levisc8/pdbDigitUtils) package can
help detect some problems with freshly digitized models. The
digitization guide has more information on how that package works.

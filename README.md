
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

## Padrino

This repo hosts the code used to generate and upload data to the
`Padrino` data base. This is more for back end management and to help
COMPADRINOs (we need a better name for this one) to find their way as
they get started. I’ve created a separate repo called `RPadrino` that
will be eventually become the equivalent of `Rcompadre`.

Currently, the data base is an Excel file with 11 tables and consists of
ASCII representation of model formulae, the coefficients associated with
said models, and the upper/lower bounds of the state variables. We are
closing in on a finalized design, at which point it seems likely that
this will transition to an SQL instance hosted remotely. Eventually, it
would be nice to be able to generate `.rda` objects (a la Compadre)
and/or query it directly from R through an API.

For now, all commits will go to the `master` branch as this is still so
early in development that keeping a separate `devel` branch is
pointless. This will change immediately before/after the first major
release.

If you think of others who could/should contribute, please let me know
and I’ll add them!

Development of the package `RPadrino` is taking place over
[here](https://github.com/levisc8/RPadrino).

## For Compadrinos

The digitization guide and other help files are located in
`metadata/digitization`. Diagnostic outputs from each run of
`R/01_create_prod_db.R` are sent to `metadata/build_artefacts`. This
folder is structured as follows:

1.  `failing_models.csv`: failing models are ones that break during the
    `make_proto_ipm` stage. These are completely unusable without
    alteration to the database itself.

2.  `unreliable_models.csv`: these models can be built using `make_ipm`,
    but have missed their test target by an amount that exceeds our
    precision threshold. In most cases, they are considerably off,
    though sometimes they miss by an amount that probably is just due to
    rounding error. These can be added back in manually by adjusting the
    `TestTargets` tab in the database itself and re-running the build
    script.

3.  `test_targets_needed.csv`: these models can also be built using
    `make_ipm`, but we don’t have any information on a numerical target
    to test against. Their reliability is unknown, and we would like to
    find some numerical quantatity that we can compute that tells us
    whether the model is behaving like its published form.

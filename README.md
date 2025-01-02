# workflow.pacta.dashboard

## Quickstart

`docker compose up --build`, with the datasets noted below in locations defined in `docker-compose.yml` (or `docker-compose.override.yml`) will build the latest version of this image, and prepare the dashboard (requires internet connection to download dependencies from GitHub/ghcr).

NOTE:
Although this application can be run locally, it is primarily intended to be run as a Docker image.
These instructions document using the Docker image, and running the workflow locally is left as an exercise for the reader.
Namely, the docker image includes built files from [`pacta-dashboard-svelte`](https://github.com/RMI-PACTA/pacta-dashboard-svelte/), while running locally requires accessing those files manually (using environment variables to control the path to rendered dashboard files).

This R package is expected to be run from the corresponding docker image [ghcr.io/rmi-pacta/workflow.pacta.dashboard:main](https://github.com/RMI-PACTA/workflow.pacta.dashboard/pkgs/container/workflow.pacta.dashboard/?tag=main).

## About

`workflow.pacta.dashboard` is a complement to [`workflow.pacta.webapp`](https://github.com/RMI-PACTA/workflow.pacta.webapp), as a drop-in replacement, with the exception of creating a PACTA interactive report, it creates a rendered dashboard showing the same results.

## Setup

### Overview

The application's deployment configuration (including for running locally) is by default controlled with environment variables (although this can be overridden by using non-default arguments to function calls).
Largely these config-options are centered around paths to directories which contain, or are expected to recieve data files.
See "Prerequisite Data" and "Application Config" (below) for more details.

The behavior of the application code for a particular run of the application (rather than a deployment) is controlled via the command-line (JSON) parameters that are passed in when the application is invoked with `Rscript` or `R --args`.
See "Command-line Parameters (`params`)" for more details.

"### Prerequisite Data

Running the application requires access to a number of prepared datasets, which should be accesible to the Docker image through bind mounts.
Each of the data sets listed can live in their own directory for clarity, or they can reside in the same directory for simplicity.
The application is configured by setting the value of enironment variables to point to the path of the bind mound *as referenced inside the container* (the `target` of the volume).

- `BENCHMARKS_DIR`:
  Outputs of [`workflow.prepare.pacta.indices`](https://github.com/RMI-PACTA/workflow.prepare.pacta.indices).
  (May be read only)
- `PACTA_DATA_DIR`:
  Outputs of [`workflow.data.preparation`](https://github.com/RMI-PACTA/workflow.data.preparation).
  Note that `workflow.data.preparation` prepares data for a given holdings date (denoted by strings such as `2022Q4` or `2023Q4`), so running this application for different portfolios may require mounting different directories.
  (May be read only)

#### Alternate run configuration

It is also possible to place a prepared set of analysis outputs (results of `workflow.pacta`) in `$ANALYSIS_OUTPUT_DIR`, and `run_dashboard_workflow(..., run_analysis = FALSE)` (see `inst/extdata/scripts/prepare_dashboard_data.R`), which is considerably faster, since it only runs the steps to translate the analysis outputs to the dashboard format.
**If doing this, `BENCHMARKS_DIR` and `PACTA_DATA_DIR` may be ignored.**
In either case, `ANALYSIS_OUTPUT_DIR` will hold the results of `workflow.pacta` after running this workflow (since it automatically runs that process if results are not found).

- (Alternate) `ANALYSIS_OUTPUT_DIR`:
  Outputs of [`workflow.pacta`](https://github.com/RMI-PACTA/workflow.pacta).
  (May be read only)

### Application Config

The following environment variables must be set.

- `ANALYSIS_OUTPUT_DIR`:
  Suggested value: `/mnt/analysis_output_dir`.
  This holds the outputs from [`workflow.pacta`](https://github.com/RMI-PACTA/workflow.pacta)
  *MUST* point to a directory that is writable by the `workflow-pacta-webapp` user if running analysis (see "Alternate run configuration" above)
- `BENCHMARKS_DIR`:
  Suggested value: `/mnt/benchmarks_dir`.
  See [Prerequisite Data](#prerequisite-data) for interpretation.
- `OUTPUT_DIR`:
  Suggested value: `/mnt/analysis_output_dir`.
  See [Prerequisite Data](#prerequisite-data) for interpretation.
- `PACTA_DATA_DIR`:
  Suggested value: `/mnt/pacta-data`.
  See [Prerequisite Data](#prerequisite-data) for interpretation.
- `PORTFOLIO_DIR`:
  Suggested value: `/mnt/portfolios`.
  This is the directory in which portfolio `.csv` files reside.
  *Note*: The application does *not* do a recursive search, so if the application is searching for `foo.csv` in `/mnt/bar`, then the portfolio must be at `/mnt/bar/foo.csv`, not `/mnt/bar/bax/foo.csv`
- `DASHBOARD_OUTPUT_DIR`:
  Suggested value: `/mnt/dashboard_output_dir`.
  This holds the dashboard `index.html` and friends, output from [`workflow.pacta.dashboard`](https://github.com/RMI-PACTA/workflow.pacta.dashboard)
  *MUST* point to a directory that is writable by the `workflow-pacta-webapp` user.
- `DASHBOARD_DATA_DIR`:
  Suggested value: `/mnt/dashboard_output_dir/data`.
  This holds the prepared JSON files that are read by the dashboard skeleton to populate plots and other elements for the user.
  *MUST* point to a directory that is writable by the `workflow-pacta-webapp` user.

The following envrionment variables are *optional*

- `LOG_LEVEL`: Controls the verbosity of logging.
  Accepts standard `log4j` levels (`ERROR`, `WARN`, `INFO`, `DEBUG`, `TRACE`).
  Default is `INFO`.

The following environment variable is option if running from the docker image, but must be set if running locally:

- `DASHBOARD_SKELETON_FILES_DIR`:
  Suggested value (in docker image): `/mnt/dashboard_skeleton_files`.
  Suggested value (local): `<path to pacta-dashboard-svelte repo >/build`.
  This holds the rendered (`build/` directory) files from `pacta-dashboard-svelte`.

## Command-line Parameters (`params`)

`run_dashboard_workflow()`'s first argument is `params`, a JSON string.
By default, this is read from the command line, so common invoca tion patterns are:

```sh
Rscript inst/extdata/scripts/prepare_dashboard_data.R {<params string>}
```

or for interactive sessions:

```sh
R --args {<params string>}
```

Very early in the `run_dashboard_workflow` process, the JSON string is validated against a JSON schema (available at `inst/extdata/schema/reportingParameters.json`).
The key elements tto control behavior of the dashboard outputs (rather than the analysis outputs) are in the `reporting` top-level key.
The parameter parsing and validation process (`pacta.workflow.utils::parse_raw_params()`) allows for inheretence of default parameters (available in `inst/extdata/parameters/*.json`), and runs a (fast initial) validation of the raw parameter string (against `inst/extdata/schema/rawParameters.json`).

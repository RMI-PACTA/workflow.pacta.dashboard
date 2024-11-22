# workflow.pacta.dashboard

In this preliminary state, this repo has a single R script [main.R](./main.R) that demonstrates generating the necessary JSON data files to drive the plots in the future PACTA "dashboard" from the outputs of a run of something like workflow.pacta. It is intended to be a thin translation layer between "running PACTA" and getting the standard "results", e.g. `audit_file.rds` and `Equity_results_portfolio.rds`, and a static(-ish) webpage that has JavaScript plots that need only to load data from JSON to work properly. To that end, the script only loads the data it needs (or sets it inline if it's not yet clear where it should get the data from) and then transforms it appropriately and exports it as proper JSON files as needed.

To do the transformations, it leverages functions that already exist in pacta.portfolio.report which are designed to do exactly this task, though in a different context. It could be that in the future those functions are re-implemented here or elsewhere, specifically for this context.

The script has three hard-coded paths that should be updated to be appropriate for the context in which it is run (eventually, these will likely be parameterized in something like a YML, JSON, or a .env file depending on whose preference prevails)
- `input_dir` a path to the directory that contains the relevant results files from a run of PACTA
- `output_dir` a path to the directory where the exported JSON files should be saved
- `data_dir`  a path to the directory that contains the PACTA data inputs files (conceptually, this should be the exact same PACTA inputs that were used to generate the PACTA results); this is currently needed for the indices/benchmarks and peer files, but those could be made available to this script in some other way in the future

The files currently needed from the `input_dir` are:
- `audit_file.rds`
- `Equity_results_portfolio.rds`
- `Bonds_results_portfolio.rds`
- `Equity_results_company.rds`
- `Bonds_results_company.rds`
- `manifest.json`
If/when more plots are added to the dashboard, other files from `input_dir` may also be needed.

The `manifest.json` is currently used to extract the following parameter values from the PACTA run that created the results in `input_dir` (in the future, these parameters could be passed to this script in some other intentional way):
- `start_year`
- `year_span`
- `pacta_sectors`
- `equity_market_levels`
- `scen_geo_levels`

It currently uses the following files from the `data_dir`:
- `Indices_bonds_results_portfolio.rds`
- `Indices_equity_results_portfolio.rds`
It could also hypothetically pull the necessary peer files from `data_dir`, but since our process for adding the peer files to the PACTA inputs directory is completely automated and always done on-the-fly in CI/CD, it's not easy to get a PACTA inputs directory locally accessible that has the peer files without manually reconstructing it, so for now fake/null peer files are included using the utility function `pacta.portfolio.utils::empty_portfolio_results()`

The following parameters are also necessary, but it is currently unclear how or where this script would get these values, so they are hard-coded to defaults (these are generally user/portfolio level parameters/options):
- `investor_name`
- `portfolio_name`
- `peer_group`
- `language_select`
- `currency_exchange_value`
- `display_currency`
- `select_scenario_other`
- `select_scenario`
- `green_techs`
- `tech_roadmap_sectors`
- `all_tech_levels`
Some of these parameters may not be strictly necessary and could be ignored if the data transformation functions were re-implemented and adjusted specifically for this context, e.g. `investor_name` and `portfolio_name` are likely unnecessary holdovers from a former time when those functions had to deal with the possibility of multiple portfolios being processed at the same time.

## Running using Docker

First, you will need a `.env` file with:
```
INPUT_DIR=/path/to/input/dir
DATA_DIR=/path/to/data/dir
OUTPUT_DIR=/path/to/output/dir
```

Then, you can run the script using `docker-compose`:
```
docker-compose up --build
```

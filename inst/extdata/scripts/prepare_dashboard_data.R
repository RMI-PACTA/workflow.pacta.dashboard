logger::log_threshold(Sys.getenv("LOG_LEVEL", "INFO"))

raw_params <- commandArgs(trailingOnly = TRUE)
params <- pacta.workflow.utils::parse_raw_params(
  json = raw_params,
  inheritence_search_paths = c(
    system.file(
      "extdata", "parameters",
      package = "workflow.pacta.dashboard"
      ),
    system.file(
      "extdata", "parameters",
      package = "workflow.pacta"
      )
  ),
  schema_file = system.file(
    "extdata", "schema", "reportingParameters.json",
    package = "workflow.pacta.dashboard"
  ),
  raw_schema_file = system.file(
    "extdata", "schema", "rawParameters.json",
    package = "workflow.pacta.dashboard"
  ),
  force_array = c("portfolio", "files")
)

manifest_info <- workflow.pacta.dashboard:::run_dashboard_workflow(
  params = params,
  raw_params = raw_params
)

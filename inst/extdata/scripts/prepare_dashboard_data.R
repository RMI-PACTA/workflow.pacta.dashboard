logger::log_threshold(Sys.getenv("LOG_LEVEL", "INFO"))

schema_tempdir <- tempdir()
workflow.pacta.dashboard:::prepare_schema_files(
  directory = schema_tempdir
)

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
  schema_file = file.path(schema_tempdir, "reportingParameters.json"),
  raw_schema_file = file.path(schema_tempdir, "rawParameters.json"),
  force_array = c("portfolio", "files")
)

manifest_info <- workflow.pacta.dashboard:::run_dashboard_workflow(
  params = params,
  raw_params = raw_params
)

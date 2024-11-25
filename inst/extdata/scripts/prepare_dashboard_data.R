logger::log_threshold(Sys.getenv("LOG_LEVEL", "INFO"))

# raw_params <- commandArgs(trailingOnly = TRUE)
# params <- pacta.workflow.utils::parse_raw_params(
#   json = raw_params,
#   inheritence_search_paths = system.file(
#     "extdata", "parameters",
#     package = "workflow.pacta.report"
#   ) ,
#   schema_file = system.file(
#     "extdata", "schema", "reportingParameters.json",
#     package = "workflow.pacta.report"
#   ),
#   raw_schema_file = system.file(
#     "extdata", "schema", "rawParameters.json",
#     package = "workflow.pacta.report"
#   ),
#   force_array = c("portfolio", "files")
# )

dashboard_manifest_info <- workflow.pacta.dashboard:::prepare_pacta_dashboard_data(
  # params = params
)

# pacta.workflow.utils::export_manifest(
#   input_files = manifest_info[["input_files"]],
#   output_files = manifest_info[["output_files"]],
#   params = manifest_info[["params"]],
#   manifest_path = file.path(Sys.getenv("REPORT_OUTPUT_DIR"), "manifest.json"),
#   raw_params = raw_params
# )

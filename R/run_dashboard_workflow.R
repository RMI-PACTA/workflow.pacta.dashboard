#' Run Dashboard workflow
#'
#' Run steps required to prepare a PACTA analysis and dashboard
#'
#' @param params (`list`) A list of parameters to be used in the analysis and
#' dashboard process. See JSON Schema for details.
#' @param raw_params (`character`) Raw JSON string of parameters.
#' @param run_analysis (`logical`) Run the analysis process.
#' @param run_dashboard (`logical`) Run the dashboard creation process.
#' @param analysis_output_dir Directory containing the PACTA analysis results.
#' @param benchmarks_dir filepath: Directory containing the benchmark analysis
#' results.
#' @param pacta_data_dir filepath: Directory with "pacta-data"
#' @param portfolio_dir filepath: Directory with portfolio files
#' @param dashboard_output_dir Directory where the dashboard will be
#' saved.
#' @return (invisible) `TRUE` if the workflow was successful. Primarily called
#' for side effect of rendering files.
#' @export
run_dashboard_workflow <- function(
  params,
  raw_params,
  run_analysis = TRUE,
  run_dashboard = TRUE,
  analysis_output_dir = Sys.getenv("ANALYSIS_OUTPUT_DIR"),
  benchmarks_dir = Sys.getenv("BENCHMARKS_DIR"),
  pacta_data_dir = Sys.getenv("PACTA_DATA_DIR"),
  portfolio_dir = Sys.getenv("PORTFOLIO_DIR"),
  dashboard_output_dir = Sys.getenv("DASHBOARD_OUTPUT_DIR")
) {

  log_trace("preparing manifest paths.")
  analysis_manifest_path <- file.path(
    analysis_output_dir,
    "manifest.json"
  )
  log_trace("Analysis manifest path: ", analysis_manifest_path)
  dashboard_manifest_path <- file.path(
    dashboard_output_dir,
    "manifest.json"
  )
  log_trace("Dashboard manifest path: ", dashboard_manifest_path)

  if (run_analysis || !file.exists(analysis_manifest_path)) {

    log_trace("running analysis workflow.")
    analysis_manifest_info <- workflow.pacta::run_pacta(
      params = params,
      pacta_data_dir = pacta_data_dir,
      output_dir = analysis_output_dir,
      portfolio_dir = portfolio_dir
    )

    log_trace("exporting analysis manifest.")
    pacta.workflow.utils::export_manifest(
      input_files = analysis_manifest_info[["input_files"]],
      output_files = analysis_manifest_info[["output_files"]],
      params = analysis_manifest_info[["params"]],
      manifest_path = analysis_manifest_path,
      raw_params = raw_params
    )

  }

  if (run_dashboard) {

    log_trace("running dashboard workflow.")
    dashboard_manifest_info <- build_dashboard(
      params = params
    )

    log_trace("exporting dashboard manifest.")
    pacta.workflow.utils::export_manifest(
      input_files = dashboard_manifest_info[["input_files"]],
      output_files = dashboard_manifest_info[["output_files"]],
      params = dashboard_manifest_info[["params"]],
      manifest_path = dashboard_manifest_path,
      raw_params = raw_params
    )

  }
  return(invisible(TRUE))
}


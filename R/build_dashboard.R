build_dashboard <- function(
  params,
  analysis_output_dir = Sys.getenv("ANALYSIS_OUTPUT_DIR"),
  benchmarks_dir = Sys.getenv("BENCHMARKS_DIR"),
  dashboard_data_dir = Sys.getenv("DASHBOARD_DATA_DIR"),
  dashboard_skeleton_files_dir = Sys.getenv("DASHBOARD_SKELETON_FILES_DIR"),
  dashboard_output_dir = Sys.getenv("DASHBOARD_OUTPUT_DIR")
) {

  prepare_pacta_dashboard_data(
    params = params,
    analysis_output_dir = analysis_output_dir,
    dashboard_data_dir = dashboard_data_dir,
    benchmarks_dir = benchmarks_dir
  )

  log_info("Copying dashboard files.")
  dashboard_copy_success <- copy_dashboard_files(
    dashboard_skeleton_files_dir = dashboard_skeleton_files_dir,
    dashboard_output_dir = dashboard_output_dir
  )
  stopifnot(dashboard_copy_success)

  dashboard_output_data_dir <- file.path(dashboard_output_dir, "data")
  if (dashboard_data_dir != dashboard_output_data_dir) {
    log_info("Copying dashboard data to correct location.")
    if (!dir.exists(dashboard_output_data_dir)) {
      dir.create(dashboard_output_data_dir, recursive = TRUE)
    }
    data_copy_success <- file.copy(
      from = dashboard_data_dir,
      to = dashboard_output_data_dir,
      recursive = TRUE
    )
    stopifnot(data_copy_success)
  }

  out <- list(
    input_files = c(
      list.files(
        dashboard_data_dir,
        full.names = TRUE,
        recursive = TRUE
      ),
      list.files(
        dashboard_skeleton_files_dir,
        full.names = TRUE,
        recursive = TRUE
      ),
      list.files(
        analysis_output_dir,
        full.names = TRUE,
        recursive = TRUE
      ),
      list.files(
        benchmarks_dir,
        full.names = TRUE,
        recursive = TRUE
      )
    ),
    output_files = c(
      list.files(
        dashboard_output_dir,
        full.names = TRUE,
        recursive = TRUE
      )
    ),
    params = params
  )

  return(out)

}

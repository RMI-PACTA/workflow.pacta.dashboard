build_dashboard <- function(
  params,
  analysis_output_dir = Sys.getenv("ANALYSIS_OUTPUT_DIR"),
  benchmarks_dir = Sys.getenv("BENCHMARKS_DIR"),
  dashboard_data_dir = Sys.getenv("DASHBOARD_DATA_DIR"),
  dashboard_skeleton_files_dir = Sys.getenv("DASHBOARD_SKELETON_FILES_DIR"),
  dashboard_output_dir = Sys.getenv("DASHBOARD_OUTPUT_DIR")
) {

  prepare_pacta_dashboard_data(
    analysis_output_dir = analysis_output_dir,
    dashboard_data_dir = dashboard_data_dir,
    benchmarks_dir = benchmarks_dir
  )

  dashboard_copy_success <- copy_dashboard_files(
    dashboard_skeleton_files_dir = dashboard_skeleton_files_dir,
    dashboard_output_dir = dashboard_output_dir
  )

  dashboard_output_data_dir <- file.path(dashboard_output_dir, "data")
  if (dashboard_data_dir != dashboard_output_data_dir) {
    if (!dir.exists(dashboard_output_data_dir)) {
      dir.create(dashboard_output_data_dir, recursive = TRUE)
    }
    data_copy_success <- file.copy(
      from = dashboard_data_dir,
      to = dashboard_output_data_dir,
      recursive = TRUE
    )
  }

}

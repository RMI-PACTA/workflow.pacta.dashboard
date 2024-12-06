copy_dashboard_files <- function(
  dashboard_files_dir = Sys.getenv("DASHBOARD_FILES_DIR"),
  dashboard_output_dir = Sys.getenv("DASHBOARD_OUTPUT_DIR")
) {

  dashboard_files <- list.files(
    dashboard_files_dir,
    full.names = TRUE,
    recursive = FALSE
  )

  dashboard_copy_success <- file.copy(
    from = dashboard_files,
    to = file.path(dashboard_output_dir),
    recursive = TRUE
  )

  return(all(dashboard_copy_success))
}

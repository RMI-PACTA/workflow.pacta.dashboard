#' Copy parameter files to directory
#'
#' Copy schema files from dependency packages to a (temporary) directory. This
#' is required, as `jsonvalidate` requires referenced schema files to exist in
#' the same directory as the referencing schema.
#'
#' @param directory Directory to copy parameter files to.
#' @return directory
#'
#' @export
prepare_schema_files <- function(directory) {
  log_debug("Preparing schema files.")
  log_debug("Schema tempdir: ", schema_tempdir)
  dashboard_schema_files <- list.files(
    system.file(
      "extdata", "schema",
      package = "workflow.pacta.dashboard"
    ),
    full.names = TRUE
  )
  dashboard_schema_copied <- file.copy(
    from = dashboard_schema_files,
    to = file.path(
      directory,
      basename(dashboard_schema_files)
    )
  )

  portfolio_schema_copied <- file.copy(
    from = system.file(
      "extdata", "schema", "portfolio.json",
      package = "workflow.pacta"
    ),
    to = file.path(
      directory,
      "portfolio.json"
    )
  )
  portfolio_schema_copied <- file.copy(
    from = system.file(
      "extdata", "schema", "portfolioParameters.json",
      package = "workflow.pacta"
    ),
    to = file.path(
      directory,
      "portfolioParameters.json"
    )
  )
  stopifnot(
    dashboard_schema_copied,
    portfolio_schema_copied
  )
  return(directory)
}

#' Scrape and export an RFA sqlite project
#'
#' Connects to an RFA project sqlite file and exports all input data,
#' VFC parameters, analyses, reservoir models, and simulation results to
#' organized CSV files within a project-named directory.
#'
#' @param sqlite_path Path to the RFA project `.rfa.sqlite` file.
#' @param base_dir The parent directory where the project export folder
#' will be created. Default is the current working directory (from `getwd()`).
#'
#' @returns The file path of the top-level project export directory.
#'
#' @examples
#' \dontrun{
#' scrape_rfa_sqlite("path/to/rfa_proejct.rfa.sqlite", "C:/export_location")
#' }
#'
#' @export
scrape_rfa_sqlite <- function(sqlite_path, base_dir = getwd()) {
  con <- DBI::dbConnect(RSQLite::SQLite(), sqlite_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  data_levels <- get_data_levels(con)
  project_name <- get_project_name(con, data_levels)
  project_dir <- make_project_folder(project_name, base_dir)
  export_2inputdata(con, project_dir)
  export_vfc_parameters(con, project_dir)
  export_3analyses(con, project_dir)
  export_4resmodel(con, project_dir)
  export_5results(con, project_dir)
  project_dir
}

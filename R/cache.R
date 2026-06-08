configure_data_cache <- function() {
  cache_dir <- Sys.getenv("SPANISH_OD_DATA_DIR", unset = "data-cache")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  Sys.setenv(SPANISH_OD_DATA_DIR = normalizePath(cache_dir, mustWork = TRUE))
}

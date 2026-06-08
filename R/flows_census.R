#' Fetch ECEPOV Benchmark data
fetch_benchmarks <- function() {
  # Fetch the municipal Place of Work/Study daily mobility table from ECEPOV-2021
  ineapir::get_data_table(idTable = 55375, unnest = TRUE, tip = "AM", metanames = TRUE, metacodes = TRUE)
}

#' String normalization helper for matching
normalize_name <- function(name) {
  name <- tolower(name)
  name <- gsub("[áàäâ]", "a", name)
  name <- gsub("[éèëê]", "e", name)
  name <- gsub("[íìïî]", "i", name)
  name <- gsub("[óòöô]", "o", name)
  name <- gsub("[úùüû]", "u", name)
  name <- gsub("ñ", "n", name)
  name <- gsub("ç", "c", name)
  name <- gsub("[^a-z0-9]", "", name)
  name
}

#' Clean and Match Benchmarks to Zones
clean_benchmarks <- function(benchmark_raw, zones_raw) {
  # Filter ECEPOV data for Ambos Sexos, Total age group, and Total commuting
  bench_filtered <- benchmark_raw |>
    dplyr::filter(
      sexo == "Ambos Sexos",
      edad == "Total",
      lugardetrabajoestudio == "Total"
    ) |>
    dplyr::select(municipios, Valor)

  # Extract zone IDs and names from zones_raw
  zones_muni <- zones_raw |>
    sf::st_drop_geometry() |>
    dplyr::select(id, name)

  # Fix encoding issues in the geopackage (e.g. CalviÍ -> Calvià)
  zones_muni$name <- gsub("CalviÍ", "Calvià", zones_muni$name)

  zones_muni$norm_name <- sapply(zones_muni$name, normalize_name)
  bench_filtered$norm_name <- sapply(bench_filtered$municipios, normalize_name)

  # Match to official zone IDs
  matched_bench <- bench_filtered |>
    dplyr::inner_join(zones_muni, by = "norm_name") |>
    dplyr::transmute(
      origin = id,
      target = as.numeric(Valor)
    )

  matched_bench
}

#' Process the Microdata ZIP into an OD matrix
clean_microdata_od_matrix <- function(zip_path) {
  tmp <- tempdir()
  
  # Unzip the main file
  utils::unzip(zip_path, files = "ECEPOVadultos.zip", exdir = tmp)
  
  # Unzip the adult file
  adult_zip <- file.path(tmp, "ECEPOVadultos.zip")
  utils::unzip(adult_zip, files = "Microdatos_ECEPOV_Adultos.csv", exdir = tmp)
  
  csv_path <- file.path(tmp, "Microdatos_ECEPOV_Adultos.csv")
  
  # Read the CSV using data.table for speed
  dt <- data.table::fread(csv_path)
  
  # Clean up temp files
  unlink(adult_zip)
  unlink(csv_path)
  
  # For now, just return the first few rows so we can inspect the exact column names
  head(dt)
}

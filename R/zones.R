#' Fetch Spanish ODD Data Zones v2
#' These zones are the geographic units used in the MITMA/Orange mobile phone mobility data.
fetch_zones <- function() {
  configure_data_cache()
  spanishoddata::spod_get_zones("muni", ver = 2)
}

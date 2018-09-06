#' Get isochrone based on departure times and locations
#' Expects `rtraveltime.id` and `rtraveltime.key` options to be set with traveltime API information
#'
#' @param data tibble of type sf with point geometry for starting locations and fields
#'   `travel_time` travel time in seconds
#'   `mode_type` transportation type
#'   `id` (optional), must be unique if present the return data will be labeled by `id` values
#' @return a vector of type sf containging the isochorone polygons
#' @export
get_departure_isochrone <- function(data){
  iso_time_string <- function(x){
    strftime(as.POSIXlt(x,"UTC"),format="%Y-%m-%dT%H:%M:00Z")
  }
  headers <- c("X-Application-Id"=getOption("rtraveltime.id"),
               "X-Api-Key"=getOption("rtraveltime.key"),
               "Accept"="application/vnd.wkt+json",
               "Content-Type"="application/json")
  url="api.traveltimeapp.com/v4/time-map"

  if (!("id" %in% names(data))) {
    d <- data %>% dplyr::mutate(id=row_number())
  } else {
    d <- data
  }

  payload <- d %>%
    dplyr::bind_cols(sf::st_coordinates(sf::st_sf(.)) %>% tibble::as.tibble()) %>%
    dplyr::mutate(payload=paste0('{"id": "',.data$id,
                                 '", "coords": {"lat": ',.data$Y,',  "lng": ',.data$X,
                                 '}, "transportation": {"type": "',.data$mode_type,
                                 '"}, "departure_time": "',iso_time_string(.data$departure_time),
                                 '","travel_time": ',.data$travel_time,'}')) %>%
    pull("payload") %>%
    paste0(collapse = ", ") %>%
    paste0('{"departure_searches": [',.,']}')

  response <- httr::POST(url,config=httr::add_headers(headers),body=payload)
  result <- NULL
  if (response$status_code==200) {
    json_response <- jsonlite::fromJSON(httr::content(response,"text"))$results
    result <- tibble::tibble("id"=json_response$search_id,"geometry"=sf::st_as_sfc(json_response$shape,crs=4326)) %>%
      sf::st_sf() %>%
      dplyr::arrange(id)
    if (!("id" %in% names(data))) result <- results %>% dplyr::select(-id)
  } else {
    warning(paste0("Request failed with status code ",response$status_code))
  }
  result
}



#' @importFrom dplyr %>%
#' @importFrom rlang .data

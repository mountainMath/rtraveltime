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
  get_isochrone(data,"departure")
}
#' Get isochrone based on arrival times and locations
#' Expects `rtraveltime.id` and `rtraveltime.key` options to be set with traveltime API information
#'
#' @param data tibble of type sf with point geometry for starting locations and fields
#'   `travel_time` travel time in seconds
#'   `mode_type` transportation type
#'   `id` (optional), must be unique if present the return data will be labeled by `id` values
#' @return a vector of type sf containging the isochorone polygons
#' @export
get_arrival_isochrone <- function(data){
  get_isochrone(data,"departure")
}


#' Get isochrone based on departure or arrival times and locations
#' Expects `rtraveltime.id` and `rtraveltime.key` options to be set with traveltime API information
#'
#' @param data tibble of type sf with point geometry for starting locations and fields
#'   `travel_time` travel time in seconds
#'   `mode_type` transportation type
#'   `id` (optional), must be unique if present the return data will be labeled by `id` values
#' @param type either 'departure' or 'arrival' to indicate the type of time request
#' @return a vector of type sf containging the isochorone polygons
#' @export
get_isochrone <- function(data,type=c("departure","arrival")){
  if (length(type)!=1 || !(type %in% c("departure","arrival"))) stop(paste0("Invalid type ",type))
  iso_time_string <- function(x){
    strftime(as.POSIXlt(x,"UTC"),format="%Y-%m-%dT%H:%M:00Z")
  }
  headers <- c("X-Application-Id"=getOption("rtraveltime.id"),
               "X-Api-Key"=getOption("rtraveltime.key"),
               "Accept"="application/vnd.wkt+json",
               "Content-Type"="application/json")
  url="http://api.traveltimeapp.com/v4/time-map"

  if (!("id" %in% names(data))) {
    d <- data %>% dplyr::mutate(id=row_number())
  } else {
    d <- data
  }

  payload <- d %>%
    dplyr::bind_cols(sf::st_coordinates(sf::st_sf(.)) %>% tibble::as_tibble()) %>%
    dplyr::mutate(payload=paste0('{"id": "',.data$id,
                                 '", "coords": {"lat": ',.data$Y,',  "lng": ',.data$X,
                                 '}, "transportation": {"type": "',.data$mode_type,
                                 '"}, "',type,'_time": "',iso_time_string(.data$departure_time),
                                 '","travel_time": ',.data$travel_time,'}')) %>%
    pull("payload") %>%
    paste0(collapse = ", ") %>%
    paste0('{"',type,'_searches": [',.,']}')

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
    warning(httr::content(response)$description)
    warning(httr::content(response)$additional_info %>% unlist %>% paste0(collapse = "\n"))
  }
  result
}



#' @importFrom dplyr %>%
#' @importFrom rlang .data

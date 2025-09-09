# Function for getting weather data

library(httr)
library(jsonlite)
library(glue)

get_stadium_weather <- function(lat, lon) {
  url <- glue("https://api.open-meteo.com/v1/forecast?latitude={lat}&longitude={lon}&current_weather=true")

  res <- httr::GET(url)
  if (httr::status_code(res) != 200) {
    stop("Weather API request failed")
  }

  weather <- fromJSON(content(res, as = "text"))

  return(list(
    temp = weather$current_weather$temperature,
    wind = weather$current_weather$windspeed
  ))
}

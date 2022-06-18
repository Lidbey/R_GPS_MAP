library(shiny)
library(shinyWidgets)
library(mapboxapi)
library(leaflet)
library(httr)
library(rjson)
library(sp)
library(shinyalert)

get_places <- function(lat_lon, cat, iso_profile, iso_time){
  # tutaj robię requesta do foursquare API po miejsca
  
  url <- "https://api.foursquare.com/v3/places/search"
  api_key = "fsq35MP9T9XP6LWYPMGNOscz1C7A+tGc4iCDLTBhtNxuZW4="
  
  rad <- switch(
    iso_profile,
    "walking" = round((iso_time/60) * 6000),
    "cycling" = round((iso_time/60) * 10000),
    "driving" = round((iso_time/60) * 30000)
  )
  response <- NULL
  if(is.null(cat))
  {
    cat <- c("")
  }
  for(category in cat) {
    queryString <- list(
      ll = lat_lon, 
      radius = rad,  # ten radius to jakaś dziwna sprawa xD
      categories = category,
      limit = 50)  # do category trzeba zrobić jakiegoś switch case'a czy coś żeby z stringa zmienić na kod api 
    responseTemp <- VERB("GET", url, add_headers(Authorization = api_key), 
                         query = queryString, 
                         content_type("application/octet-stream"), 
                         accept("application/json"))
    responseJSON <- fromJSON(content(responseTemp, "text"))
    if(is.null(response))
    {
      response<- responseJSON
    }
    else
    {
      response$results <- c(response$results, responseJSON$results)
    }
  }
  return(response)
}

get_lat_lon_vect <- function(resp_pars, iso){
  
  # tutaj tworzę vector dla długości lub szerokości geograficznej miejsc z forsquara
  # po zrobieniu obu tych vectorów używam ich do tworzenia markerów na mapie
  # na pewno da się to lepiej zrobić xD
  
  # type = "lat" for latitude and "lon" for longitude
  out <- NULL
  for(result in resp_pars$results){
    if(is.null(result[["geocodes"]][["main"]]$longitude) ||
      is.null(result[["geocodes"]][["main"]]$latitude))
    {
      next;
    }
    if(check_if_inside_iso(result[["geocodes"]][["main"]]$longitude, 
                           result[["geocodes"]][["main"]]$latitude,
                           iso) == 1){
      out$lats <- c(out$lats, result[["geocodes"]][["main"]]$latitude)
      out$lons <- c(out$lons, result[["geocodes"]][["main"]]$longitude)
    }
  }
  return(out)
}

get_places_names <- function(resp_pars, iso){
  
  names <- c("Your Location")
  for(result in resp_pars[["results"]]){
    if(check_if_inside_iso(result[["geocodes"]][["main"]]$longitude, 
                           result[["geocodes"]][["main"]]$latitude,
                           iso) == 1){
      if(is.null(result[["geocodes"]][["main"]]$longitude) ||
         is.null(result[["geocodes"]][["main"]]$latitude))
      {
        next;
      }
      longlatstring = paste("\"",result[["geocodes"]][["main"]]$longitude, 
                            " ", result[["geocodes"]][["main"]]$latitude, "\"")
      name <- paste0("<b>", result$name, "</b></br>",
                     "<button onclick='Shiny.onInputChange(\"button_click\",",  longlatstring,")' 
                     id='selectlocation' type='button' 
                     class='btn btn-default action-button'>Show shortest path</button>")
      names <- c(names, name)
    }
  }
  
  return(names)
}

get_marker_colors <- function(resp_pars, iso){
  
  colors <- c("https://cdn-icons-png.flaticon.com/512/1329/1329665.png")
  
  for(result in resp_pars[["results"]]){
    if(is.null(result[["geocodes"]][["main"]]$longitude) ||
       is.null(result[["geocodes"]][["main"]]$latitude))
    {
      next;
    }
    if(check_if_inside_iso(result[["geocodes"]][["main"]]$longitude, 
                           result[["geocodes"]][["main"]]$latitude,
                           iso) == 1){
        
        if (cat_index <- length(result[["categories"]]) >=1 ){
          cat_index <- result[["categories"]][[1]]$id 
          
          if((cat_index > 13000) & (cat_index < 14000)){  # dining and drinking
            colors <- c(colors, "https://cdn-icons-png.flaticon.com/128/1329/1329641.png")
          }
          else if((cat_index > 11000) & (cat_index < 12000)){ # business and professional services
            colors <- c(colors, "https://cdn-icons-png.flaticon.com/128/1329/1329644.png")
          }
          else if((cat_index > 10000) & (cat_index < 11000)){ # arts and entertainment
            colors <- c(colors, "https://cdn-icons-png.flaticon.com/128/1329/1329632.png")
          }
          else if((cat_index > 16000) & (cat_index < 17000)){ # landmarks and outdoors
            colors <- c(colors, "https://cdn-icons-png.flaticon.com/128/1329/1329667.png")
          }
          else{
            colors <- c(colors, "https://cdn-icons-png.flaticon.com/128/1329/1329622.png")
          }
        }
    }
  }
  return(colors)  
}

check_if_inside_iso <- function(lon, lat, iso) {
  # tutaj zbieram koordynaty wielokata isochronu
  coords <- iso[[2]][[1]][[1]]
  len = length(coords)
  x_pol <- coords[1:(len/2)]
  y_pol <- coords[(len/2+1):len]
  
  # tutaj sprawdzam dla każdego miejsca czy jest w wielokacie isochronu
  # jesli tak appenduje go do do nowej listy miejsc
  return(point.in.polygon(point.x = lon, point.y = lat, pol.x = x_pol, pol.y = y_pol))
}

split_into_categories <- function(labels, lon, lat, icons, popups)
{
  output <- list()
  for(i in 1:length(unique(labels)))
  {
    output[[i]] <- list()
    output[[i]]$lons <- lon[labels==unique(labels)[i]]
    output[[i]]$lats <- lat[labels==unique(labels)[i]]
    output[[i]]$icons <- icons
    output[[i]]$icons$iconUrl <- icons$iconUrl[labels==unique(labels)[i]]
    output[[i]]$popups <- popups[labels==unique(labels)[i]]
  }
  return(output)
}

ui <- fluidPage(
  
    setBackgroundColor(
      color = c("#5FC9EB", "#0D7BBA"),
      gradient = "linear",
      direction = "bottom"
    ),

    # Application title
    titlePanel("Trip Advisor"),
    sidebarLayout(
      sidebarPanel(
        textInput("start_location", "Type start location"),
        selectInput("iso_profile", "Select isochrone profile", c("Walking" = "walking", 
                                                                   "Cycling" = "cycling", 
                                                                   "Driving" = "driving")),
        sliderInput("iso_time",
                    "Isochrone time:",
                    min = 5,
                    max = 30,
                    step = 5,
                    value = 10),
        selectInput("select", label = h3("Select categories"), 
                      list("Dining and Drinking" = 13000, 
                           "Arts and Entertainment" = 10000,
                           "Business and Professional Services" = 11000,
                           "Landmarks and Outdoors" = 16000),
                    multiple = TRUE),
        ),
       mainPanel(
          leafletOutput("my_map", width = "100%", height = 600)
       )
    )
    
)

server <- function(input, output) {
  
  coord <- reactiveValues(lon=NULL, lat=NULL)
  click <- reactiveValues(lon=NULL, lat=NULL)
  old_input <- reactiveValues(x="NULL")
  
  observe({
    click$x <- input$my_map_click$lat
    click$y <- input$my_map_click$lng
    if(!(is.null(click$x) || is.null(click$y)))
    {
      alert <- shinyalert("Change location", "Do you want to change location?",
                          confirmButtonText = "Yes", showCancelButton = TRUE,
                          cancelButtonText = "No", callbackR = function(x) {if(x!=FALSE){
                            coord$lat <- click$x
                            coord$lon <- click$y
                          }})
    }
  })
  
  observeEvent(input$button_click,{
    gps<-NULL
    longlatstr <- strsplit(input$button_click, split = " ")
    lon <- as.numeric(longlatstr[[1]][2])
    lat <- as.numeric(longlatstr[[1]][5])
    path <- mb_directions(origin = c(coord$lon, coord$lat), destination = c(lon, lat), profile = input$iso_profile)
    gps$lons <- path$geometry[[1]][1:(length(path$geometry[[1]])/2)]
    gps$lats <- path$geometry[[1]][(length(path$geometry[[1]])/2+1):length(path$geometry[[1]])]
    leafletProxy("my_map") %>%
      addPolylines(lng = gps$lons, lat = gps$lats, color = "#CC0000", layerId = 'polyline')
  })
  
  output$my_map <- renderLeaflet({
    validate(
      need(input$start_location != "" || !is.null(coord$lon), "Please choose the starting point")
    )
    coords <- mb_geocode(input$start_location)
    if(old_input$x!=input$start_location)
    {
      coord$lon <- coords[1]
      coord$lat <- coords[2]
      old_input$x <- input$start_location
    }
    iso <- mb_isochrone(c(coord$lon, coord$lat),profile = input$iso_profile, time = input$iso_time)
    
    coord_string <- sprintf("%s,%s", coord$lat, coord$lon)
    
    response_parsed <- get_places(coord_string, input$select, input$iso_profile, as.numeric(input$iso_time))
    pop_up <- get_places_names(response_parsed, iso)
    
    coordinates <- get_lat_lon_vect(response_parsed, iso)

    lon_markers <- c(coord$lon, coordinates$lons)
    lat_markers <- c(coord$lat, coordinates$lats)

    location_icons <- makeIcon(
      iconUrl = get_marker_colors(response_parsed, iso),
      iconWidth = 40, iconHeight = 40
    )
    
    #categories <- split_into_categories(iconUrls, lon_markers, lat_markers, location_icons, pop_up)
    
    #for(el in categories)
    #{
    #  leafletProxy("my_map") %>% addMarkers(el$lons, el$lats, icon=el$icons, popup=el$popups,
    #                                        clusterOptions = markerClusterOptions(disableClusteringAtZoom=15))
    #}    

    leaflet(iso) %>% 
      addMapboxTiles(style_id = "streets-v11", username = "mapbox") %>% 
      addPolygons(color = "#1F85DE") %>%
      addMarkers(lon_markers, lat_markers, icon = location_icons, popup = pop_up,
                 clusterOptions = markerClusterOptions(
                   disableClusteringAtZoom=15
                 )) %>%
      setView(coord$lon, coord$lat, zoom = 15)
  })
}

# Run the application 
options(shiny.host = '0.0.0.0')
options(shiny.port = 1099)
shinyApp(ui = ui, server = server)

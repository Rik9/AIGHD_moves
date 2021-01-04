#server code for AIGHD Moves - shiny app
library(shiny)
library(XML)
library(sp)
library(raster)
library(RColorBrewer)

#function to shift coordinates
shift.vec <- function(vec, shift) {
  if(length(vec) <= abs(shift)) {
    rep(NA ,length(vec))
  } else {
    if (shift >= 0) {
      c(rep(NA, shift), vec[1:(length(vec)-shift)]) }
    else {
      c(vec[(abs(shift)+1):length(vec)], rep(NA, abs(shift))) } 
  } 
}

#load distances of participants
link_distances <- "https://raw.githubusercontent.com/Rik9/AIGHD_moves/master/Distances.csv"
distances <- read.csv(link_distances, header = TRUE, stringsAsFactors = FALSE, sep = ",")
distances$cumsum_dist <- cumsum(distances$Distance)
n_participants <- length(unique(distances$Name))
distance_tot <- sum(distances$Distance)

#load all gpx files and create dataframe with route points for those relevant
  #created with https://mapstogpx.com and link from google maps
options(digits = 10)
stages <- list.files(pattern = "*.gpx")

#load waypoints
start_finish <- read.csv("Waypoints.csv", header=TRUE, stringsAsFactors = FALSE)

for(i in 1:length(stages)) {
  stage <- htmlTreeParse(stages[i], error = function(...) { }, useInternalNodes = T)
  
  coords <- xpathSApply(stage, path = "//trkpt", xmlAttrs)
  lats <- as.numeric(coords["lat",])
  lons <- as.numeric(coords["lon",])
  
  stage_df <- data.frame(lat = lats, lon = lons)
  rm(list=c("lats", "lons", "coords"))
  
  #shift coordinates and calculate distances between subsequent points
  stage_df$lat.p1 <- shift.vec(stage_df$lat, -1)
  stage_df$lon.p1 <- shift.vec(stage_df$lon, -1)
  
  stage_df$dist.to.prev <- apply(stage_df, 1, FUN = function(row) {
    pointDistance(c(as.numeric(row["lat.p1"]),
                    as.numeric(row["lon.p1"])),
                  c(as.numeric(row["lat"]),
                    as.numeric(row["lon"])),
                  lonlat = T)
  })
  
  #remove final point
  stage_df <- stage_df[-nrow(stage_df),]
  
  if(i == 1) {
    distance_goal <- sum(stage_df$dist.to.prev)
    stages_tot <- stage_df
  } else {
    distance_goal <- sum(distance_goal, stage_df$dist.to.prev)
    stages_tot <- rbind(stages_tot, stage_df)
  }
  
  if(distance_goal >= distance_tot) { 
    start_finish <- start_finish[1:(i+1),]
    lab_start <- paste0("Start: ", start_finish$Location[1])
    lab_finish <- paste0("Finish: ", start_finish$Location[nrow(start_finish)])
    lab_waypoints <- paste0("Waypoint: ", start_finish$Location[-c(1,nrow(start_finish))])
    if(i > 1) {
      start_finish$label <- c(lab_start, lab_waypoints, lab_finish)
    } else {
      start_finish$label <- c(lab_start, lab_finish)
    }
    coordinates(start_finish) <- ~ lon + lat
    break 
  }
}

#allocate distances to people and split if necessary
stages_tot$cumsum_dist <- cumsum(stages_tot$dist.to.prev)
stages_tot$name <- stages_tot$date <- stages_tot$mode <- NA

for(i in 1:nrow(distances)) {
  name <- distances$Name[i]
  date <- distances$Date[i]
  mode <- distances$Mode[i]
  if(i == 1) {
    dist_min <- 0
  } else {
    dist_min <- distances$cumsum_dist[i-1]
  }
  dist_max <- distances$cumsum_dist[i]
  
  #all segments to include
  incl_lines <- which(stages_tot$cumsum_dist > dist_min & stages_tot$cumsum_dist <= dist_max)
  stages_tot$name[incl_lines] <- name
  stages_tot$date[incl_lines] <- date
  stages_tot$mode[incl_lines] <- mode
  
  #if there are still unclaimed segments left:
  if(any(is.na(stages_tot$name))) {
    #characteristics of new line segment
    new_lat <- stages_tot$lat[max(incl_lines) + 1]
    new_lon <- stages_tot$lon[max(incl_lines) + 1]
    new_dist <- dist_max - stages_tot$cumsum_dist[max(incl_lines)]
    new_dist2 <- stages_tot$dist.to.prev[max(incl_lines) + 1] - new_dist #distance left
    new_cumsum_dist <- distances$cumsum_dist[i]
    new_lat.p1 <- (new_lat/new_dist + stages_tot$lat.p1[max(incl_lines) + 1]/new_dist2) / (1/new_dist + 1/new_dist2)
    new_lon.p1 <- (new_lon/new_dist + stages_tot$lon.p1[max(incl_lines) + 1]/new_dist2) / (1/new_dist + 1/new_dist2)
    
    stages_tot_new <- data.frame(lat = new_lat, lon = new_lon,
                                 lat.p1 = new_lat.p1, lon.p1 = new_lon.p1,
                                 dist.to.prev = new_dist, cumsum_dist = new_cumsum_dist,
                                 mode = mode, date = date, name = name)
    
    #define new next point coordinates
    stages_tot$dist.to.prev[max(incl_lines) + 1] <- new_dist2
    stages_tot$lat[max(incl_lines) + 1] <- new_lat.p1
    stages_tot$lon[max(incl_lines) + 1] <- new_lon.p1
    
    #rbind new stages_tot_new
    stages_tot <- rbind(stages_tot, stages_tot_new)
    stages_tot <- stages_tot[order(stages_tot$cumsum_dist),]
    row.names(stages_tot) <- NULL
  }
}

#merge segments per participant
stages_tot$ID <- ifelse(is.na(stages_tot$name),"Open",
                        paste(stages_tot$date, stages_tot$name, stages_tot$mode, sep = " "))
stages_tot_spl <- split(stages_tot,stages_tot$ID)

#list of lines
list_lines <- list()
for(i in 1:length(stages_tot_spl)) {
  #all coordinates
  crds <- stages_tot_spl[i][[1]][c(2,1)]
  #add final point
  crds[nrow(stages_tot_spl[i][[1]]) + 1,] <- stages_tot_spl[i][[1]][nrow(stages_tot_spl[i][[1]]),c(4,3)]
  #create lines
  ln <- Lines(list(Line(as.matrix(crds))),names(stages_tot_spl)[i])
  list_lines[i] <- ln
}

#label distances
row.names(distances) <- c(paste(distances$Date, distances$Name, distances$Mode, sep = " "))
distances$txt <- c(paste(distances$Date, distances$Name, distances$Mode,
                         paste0(round(distances$Distance/1000, digits = 1),"km"),
                         sep = " "))

#summed distances per participant
tot_dist <- t(rbind(by(distances$Distance,distances$Name,FUN = function(x){sum(x)})))
tot_dist_df <- data.frame(Name = row.names(tot_dist), tot_dist = tot_dist, row.names = NULL)
distances$tot_dist <- tot_dist_df$tot_dist[match(distances$Name, tot_dist_df$Name)]

#add empty row to distances data frame
distances[nrow(distances)+1,] <- NA
distances$Distance[nrow(distances)] <- round(sum(stages_tot_spl["Open"]$Open$dist.to.prev))
distances$cumsum_dist[nrow(distances)] <- round(max(stages_tot_spl["Open"]$Open$cumsum_dist))
distances$txt[nrow(distances)] <- paste("Only",paste0(round(distances$Distance[nrow(distances)]/1000,digits = 1),"km"),"to go!")
row.names(distances)[nrow(distances)] <- "Open"

#one line per activity
stages_tot_lines <- SpatialLinesDataFrame(SpatialLines(list_lines,proj4string = CRS("+init=epsg:4326")), 
                                          data = distances)


server <- function(input,output) {
  #color scheme
  factpal <-  colorFactor(colorRampPalette(brewer.pal(9,"Set1"))(n_participants), 
                          unique(stages_tot_lines@data$Name[!is.na(stages_tot_lines@data$Name)]), na.color = "black")
  
  #create empty vectors for participants
  participants <- reactiveValues(name=NULL)
  
  #create map with lines
  output$map <- renderLeaflet({
    leaflet() %>%
      addFeatures(start_finish, weight = 2, color = "grey",
                  opacity = 0.5,
                  label = ~label) %>%
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addFeatures(stages_tot_lines, weight = 2, color = ~factpal(Name),
                  opacity = 1, 
                  layerId = ~row.names(stages_tot_lines),
                  highlightOptions = highlightOptions(weight = 6),
                  label = ~txt)
    
  })
  
  #return total distance reached and still to go
  summ <- paste0(round(sum(stages_tot_lines@data$Distance[!is.na(stages_tot_lines@data$Name)])/1000, digits = 1),"km reached! ",
                 round(stages_tot_lines@data$Distance[is.na(stages_tot_lines@data$Name)]/1000, digits = 1),"km to go!")
  output$summary <- renderText(expr = summ)
  
  #select participants to highlight all their distances
  observeEvent(ignoreNULL = FALSE, input$select_participants, {
    participants$name <- input$select_participants
    
    stages_lines_inc <- stages_tot_lines[stages_tot_lines$Name%in%participants$name,]
    stages_lines_exc <- stages_tot_lines[!(stages_tot_lines$Name%in%participants$name),]
    
    if(length(stages_lines_inc) == 0) {
      leafletProxy("map") %>%
        addFeatures(start_finish, weight = 2, color = "grey",
                    opacity = 0.5,
                    label = ~label) %>%
        addFeatures(stages_tot_lines, weight = 2, color = ~factpal(Name),
                    opacity = 1,
                    layerId = ~row.names(stages_tot_lines),
                    highlightOptions = highlightOptions(weight = 6),
                    label = ~txt) 
      
      stages_plot <- stages_tot_lines@data[!is.na(stages_tot_lines@data$Name),]
    } else {
      leafletProxy("map") %>%
        addFeatures(start_finish, weight = 2, color = "grey",
                    opacity = 0.5,
                    label = ~label) %>%
        addFeatures(stages_lines_inc, weight = 6, color = ~factpal(Name),
                    opacity = 1,
                    layerId = ~row.names(stages_lines_inc),
                    highlightOptions = highlightOptions(weight = 6),
                    label = ~txt) %>%
        addFeatures(stages_lines_exc, weight = 2, color = ~factpal(Name),
                    opacity = 1,
                    layerId = ~row.names(stages_lines_exc),
                    highlightOptions = highlightOptions(weight = 6),
                    label = ~txt) 
      
      stages_plot <- stages_lines_inc@data[!is.na(stages_lines_inc@data$Name),]
    }
    
    stages_plot$Name <- factor(stages_plot$Name, levels = unique(stages_plot$Name[order(stages_plot$tot_dist)]))
    fills <- factpal(stages_plot$Name)
    
    p_bar <- ggplot(data = stages_plot, aes(x = round(Distance/1000,digits=1), y = Name)) +
      geom_bar(stat = "identity", fill = fills) +
      ggtitle("Total distance") +
      xlab("Distance (km)") +
      ylab(NULL) +
      theme_minimal()
    
    output$bar <- renderPlot(expr = p_bar)
    
  }
  )
}

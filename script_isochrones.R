#############################################################################
#                  TEST MULTI ISCOCHRONES

rgrid <- function(loc, dmax, res){
  # create a regular grid centerd on loc
  boxCoordX <- seq(from = sf::st_coordinates(loc)[1,1] - dmax,
                   to = sf::st_coordinates(loc)[1,1] + dmax,
                   length.out = res)
  boxCoordY <- seq(from = sf::st_coordinates(loc)[1,2] - dmax,
                   to = sf::st_coordinates(loc)[1,2] + dmax,
                   length.out = res)
  sgrid <- expand.grid(boxCoordX, boxCoordY)
  sgrid <- data.frame(ID = seq(1, nrow(sgrid), 1),
                      COORDX = sgrid[, 1],
                      COORDY = sgrid[, 2])
  sgrid <- sf::st_as_sf(sgrid,  coords = c("COORDX", "COORDY"),
                        crs = st_crs(loc), remove = FALSE)
  return(sgrid)
}


library(sf)
iris <- st_read("data-raw/CONTOURS-IRIS.shp", quiet = TRUE)

# Extract Paris and delete 
# Bois-de-Vincennes / Boulogne Iris for map template
iris$dep <- substr(iris$INSEE_COM, 1, 2)
paris <- iris[iris$dep == "75",]
paris <- paris[!paris$NOM_IRIS %in% c("Bois de Vincennes 1",
                                      "Bois de Vincennes 2",
                                      "Bois de Boulogne 1",
                                      "Bois de Boulogne 2",
                                      "Bois de Boulogne 3"),]
paris <- st_union(paris)

# 5 km around Paris map layout
paris5k <- st_buffer(paris, 5000)
paris5k <- st_as_sfc(st_bbox(paris5k, crs = 2154))
paris <- paris5k

# 10 km around Paris (get OSM data) in long/lat
paris10k <- st_buffer(paris, 10000)
paris10k <- st_as_sfc(st_bbox(paris10k, crs = 2154))

# Intersection with IRIS
iris10k <- st_intersection(iris, paris10k)

# Bounding box for osm extraxt
paris10k <- st_transform(paris10k, 4326)
paris10k <- st_bbox(paris10k) 
paris10k <- as.vector(paris10k)


# 3. Extract OSM objects (climbing and map layout)----
library(osmdata)

# define a bounding box
q0 <- opq(bbox = paris10k) 

# extract climbing areas
q <- add_osm_feature(opq = q0, key = 'sport', value = "climbing")
res <- osmdata_sf(q)
dest <- res$osm_points
dest[,"name"] <- iconv(dest$name, from = "UTF-8", to = "UTF-8")

# Cleaning
private <- dest[!is.na(dest$brand),] # Manage private and associative areas
asso <- dest[!is.na(dest$federation),]
asso$type <- "Associative structure"
private$type <- "Speculative structure"
dest <- rbind(asso, private)
dest$federation[is.na(dest$federation)] <- "Private"
# Find walls and boulders
dest[c("climbing.toprope", "climbing.boulder")][is.na(dest[c("climbing.toprope", "climbing.boulder")])] <- "no"
dest$climbing_type <- ifelse(dest$climbing.toprope == 'yes' & 
                               dest$climbing.boulder == "yes", 'Wall and bouldering',
                             ifelse(dest$climbing.toprope == 'yes' & 
                                      dest$climbing.boulder == "no" , 'Wall',
                                    ifelse(dest$climbing.toprope == 'no' & 
                                             dest$climbing.boulder == "yes" ,
                                           'Bouldering', NA)))
# Keep only attributes of interest and rename it
cols <- c("osm_id", "name", "climbing_type", "climbing.length",
          "climbing.routes", "type", "federation", "brand")
dest <- dest[,cols]
colnames(dest)[4:5] <- c("climbing_length", "climbing_routes")



# Isochrones (heavy calculation)
paris <- st_transform(paris, 2154)
grid <- st_make_grid(paris, cellsize = 200)
grid <- st_centroid(grid)
grid <- st_sf(ID = 1:length(grid), geometry = grid)
grid <- st_transform(grid, 4326)

library(osrm)
options(osrm.server = "http://localhost:5000/", osrm.profile = "bike")
# df5 <- osrmTable(src = grid, dst = dest, measure = "duration")
# df5 <- data.frame(df5$duration)
# colnames(df5) <- as.character(df5$osm_id)
# row.names(df5) <- as.character(grid$ID)
# write.csv(df5, "data-conso/grid-bike-duration.csv")
# st_write(grid, "data-conso/grid.gpkg")

grid <- st_read("data-conso/grid.gpkg")

df5 <- read.csv("data-conso/grid-bike-duration.csv")
colnames(df5) <- as.character(dest$osm_id)
osm_id <- colnames(df5)[apply(df5, 1, which.min)] # Name
osm_id <- data.frame(osm_id, stringsAsFactors = FALSE)
osm_id$ID <- row.names(df5)

# Time to the nearest climbing area
time <- apply(df5, 1, min) # Time
time <- data.frame(time, stringsAsFactors = FALSE)
time$ID <- row.names(time)
osm_id <- merge(osm_id, time, by = "ID", all.x = TRUE)
head(osm_id)
grid <- merge(grid, osm_id[,c("ID","time")], by = "ID", all.x = TRUE)


grid <- st_transform(grid, 2154)
grid$COORDX <- st_coordinates(grid)[,1]
grid$COORDY <- st_coordinates(grid)[,2]
grid$OUTPUT <- grid$time

#remotes::install_github("riatelab/potential")
library(potential)
equipot <- equipotential(grid, var = "time", mask = iris)

length(grid[["OUTPUT"]])
length(unique(grid[["COORDX"]]))
length(unique(grid[["COORDY"]]))

brks <- c(0, 2.5, 5, 7.5, 10, 12.5, 15, 17.5, 20, 22.5, 25, 27.5, max(grid$time))
head(grid)
mf_map(x = grid, var = "OUTPUT", type = "choro", 
       breaks = brks, border = NA,
       leg_pos = "topleft", leg_frame = F,
       leg_title = "Isochrones\n(min)",
       leg_title_cex = 1, leg_val_cex = .8, 
       add = F)

library(mapsf)
mf_map(grid)

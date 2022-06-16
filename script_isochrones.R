#############################################################################
#                  TEST MULTI ISCOCHRONES


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
paris0 <- paris

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
mygrid <- st_make_grid(paris5k, cellsize = 150)
mygrid <- st_centroid(mygrid)
mygrid <- st_sf(ID = 1:length(mygrid), geometry = mygrid)

mygrid$COORDX <- st_coordinates(mygrid)[,1]
mygrid$COORDY <- st_coordinates(mygrid)[,2]

dest <- st_transform(dest, 2154)

head(mygrid)
head(dest)

library(osrm)
options(osrm.server = "http://localhost:5000/", osrm.profile = "bike")
df5 <- osrmTable(src = mygrid, dst = dest, measure = "duration")


# df5 <- osrmTable(src = grid, dst = dest, measure = "duration")
df5 <- data.frame(df5$duration)
colnames(df5) <- as.character(dest$osm_id)
row.names(df5) <- as.character(mygrid$ID)
# write.csv(df5, "data-conso/grid-bike-duration.csv")
# st_write(grid, "data-conso/grid.gpkg")

quantile(mygrid$time, probs = seq(0,1,0.1))

grid <- st_read("data-conso/grid.gpkg")
df5 <- read.csv("data-conso/grid-bike-duration.csv")
df5 <- df5[,-1]

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
mygrid <- merge(mygrid, osm_id[,c("ID","time")], by = "ID", all.x = TRUE)



# remotes::install_github("riatelab/potential")
library(potential)

osrm:::isopoly()

mygrid$COORDX <- st_coordinates(mygrid)[,1]
mygrid$COORDY <- st_coordinates(mygrid)[,2]
brks <- c(0, 1, 2.5, 5,  10, 15, 20,30,max(mygrid$time))
head(mygrid)
iso <- equipotential(x = mygrid, var = "time", breaks = brks,  mask = paris5k,
                     xcoords = "COORDX", ycoords = "COORDY")





iso <- osrm:::isopoly(x = mygrid, var = "time", breaks = brks, mask = paris5k)





head(paris0)

cols <- mf_get_pal(n = c(4,4), pal = c("Reds 2", "Greens"))

mf_map(paris5k, col = NA, add = T)

head(iso)
head(paris5k)

library(mapsf) 
mf_map(iso, var = "center", type = "choro", breaks = brks, pal = cols)

library(rmapshaper)
iso <- ms_simplify(iso, keep = .99)


iso <- st_make_valid(iso)

head(iso)


 ??tanaka
 
 osrmIsochrone

library(mapsf)
mf_map(grid, var = "time", type = "choro")
mf_map(dest, pch = 21, bg = "red", add = TRUE)


# Fix pb
x = grid
var = "time"
xcoords = "COORDX"
ycoords = "COORDY"

length(unique(x[[xcoords]]))
length(unique(x[[ycoords]]))
441*470
length(levels(as.factor(x[[xcoords]])))
length(levels(as.factor(x[[ycoords]])))
152*131




m <- matrix(
  data = x[[var]], nrow = length(unique(x[[xcoords]])),
  dimnames = list(unique(x[[xcoords]]), unique(x[[ycoords]]))
)

m <- matrix(
  data = x[[var]], nrow = length(unique(x[[xcoords]])),
  dimnames = list(unique(x[[xcoords]]), unique(x[[ycoords]]))
)






# Vignette
y <- create_grid(x = n3_poly, res = 100000)
d <- create_matrix(x = n3_pt, y = y)
y$pot <- potential(x = n3_pt, y = y, d = d,
                   var = "POP19", fun = "e",
                   span = 75000, beta = 2)
y$pot2 <- 100 * y$pot / max(y$pot)
iso <- equipotential(x = y, var = "pot2", breaks = seq(0,100, 10), mask = n3_poly)


length(unique(y[["COORDX"]]))
length(unique(y[["COORDY"]]))

nrow(y)
49*42

dimnames(y)[2]
head(y)

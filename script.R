# 1. Map layout preparation at IRIS scale (source IGN)----
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

# 10 km around Paris (get OSM data) in long/lat
paris10k <- st_buffer(paris, 10000)
paris10k <- st_as_sfc(st_bbox(paris10k, crs = 2154))

# Intersection with IRIS
iris10k <- st_intersection(iris, paris10k)

# Bounding box for osm extraxt
paris10k <- st_transform(paris10k, 4326)
paris10k <- st_bbox(paris10k) 
paris10k <- as.vector(paris10k)

# Communes aggregation (layout)
com <- aggregate(iris[,c("INSEE_COM", "NOM_COM")],
                 by = list(iris$INSEE_COM),
                 FUN = head, 1)


# 2. Feed IRIS layer by socio-economic data (INSEE) ----
library(readxl)
df <- read_xlsx("data-raw/base-ic-evol-struct-pop-2018.xlsx", skip = 5, sheet = "IRIS")
iris10k <- merge(iris10k[,c("CODE_IRIS", "NOM_IRIS", "TYP_IRIS", "NOM_COM")], 
              df[,c("IRIS","P18_POP")],
              by.x = "CODE_IRIS", by.y = "IRIS", all.x = TRUE)

df <- read_xlsx("data-raw/BASE_TD_FILO_DISP_IRIS_2019.xlsx", skip = 5, sheet = "IRIS_DISP")
iris10k <- merge(iris10k, df[,c("IRIS","DISP_MED19")],
              by.x = "CODE_IRIS", by.y = "IRIS", all.x = TRUE)

iris <- st_intersection(iris, paris5k)

# Keep only habitation IRIS for origins calculation
ori <- iris10k[iris10k$TYP_IRIS == "H",]
ori <- st_centroid(ori)

ori <- st_transform(ori, crs = 4326)
iris <- st_transform(iris, crs = 4326)
com <- st_transform(com, crs = 4326)


# 3. Extract sport climbing areas from OSM ----
library(osmdata)

# define a bounding box
q0 <- opq(bbox = paris10k) 

# extract climbing areas
q <- add_osm_feature(opq = q0, key = 'sport', value = "climbing")
res <- osmdata_sf(q)
dest <- res$osm_points

# Manage private and associative areas
private <- dest[!is.na(dest$brand),]
asso <- dest[!is.na(dest$federation),]
asso$type <- "Associative structure"
private$type <- "Speculative structure"
dest <- rbind(asso, private)

cols <- c("osm_id", "name", "climbing.toprope", "climbing.boulder", "climbing.length",
          "climbing.routes", "type", "federation", "brand")
dest <- dest[,cols]
dest$federation[is.na(dest$federation)] <- "Private"

poi <- st_transform(dest, 2154)
poi <- st_intersection(poi, paris5k)
poi <- st_transform(poi, 4326)

# 4. Origin - Destination calculation with OSRM ----
library(osrm)

# Manage ids
row.names(ori) <- ori$CODE_IRIS

# Connexion to osrm local server
options(osrm.server = "http://localhost:5000/", osrm.profile = "bike")

# Origin-destination calculation
# All structures
df <- osrmTable(src = ori, dst = dest, measure = "duration")
df <- data.frame(df$duration)
colnames(df) <- as.character(dest$osm_id)
row.names(df) <- as.character(ori$CODE_IRIS)
write.csv(df, "data-conso/bike-duration.csv")

# Private structures
dest_priv <- dest[dest$type == "Speculative structure",] 
df2 <- osrmTable(src = ori, dst = dest_priv, measure = "duration")
df2 <- data.frame(df2$duration)
colnames(df2) <- as.character(dest_priv$osm_id)
row.names(df2) <- as.character(ori$CODE_IRIS)
write.csv(df2, "data-conso/bike-duration-priv.csv")

# FFME
dest_asso <-dest[dest$type == "Associative structure",] 
dest_ffme <- dest_asso[dest_asso$federation == "FFME",]
df3 <- osrmTable(src = ori, dst = dest_ffme, measure = "duration")
df3 <- data.frame(df3$duration)
colnames(df3) <- as.character(dest_ffme$osm_id)
row.names(df3) <- as.character(ori$CODE_IRIS)
write.csv(df3, "data-conso/bike-duration-ffme.csv")

# FSGT
dest_fsgt <- dest_asso[poi_asso$federation == "FSGT",]
df4 <- osrmTable(src = ori, dst = dest_fsgt, measure = "duration")
df4 <- data.frame(df4$duration)
colnames(df4) <- as.character(poi_fsgt$osm_id)
row.names(df4) <- as.character(ori$CODE_IRIS)
write.csv(df4, "data-conso/bike-duration-fsgt.csv")


# 5. Accessibility indicator creation (IRIS) ----
# Name of the nearest structure
osm_id <- colnames(df)[apply(df, 1, which.min)] # Name
osm_id <- data.frame(osm_id, stringsAsFactors = FALSE)
osm_id$iris <- row.names(df)
osm_id <- merge(osm_id, poi[,c("osm_id", "name", "type")], 
                by = "osm_id", all.x = TRUE)

# Time to the nearest climbing area
time <- apply(df, 1, min) # Time
time <- data.frame(time, stringsAsFactors = FALSE)
time$iris <- row.names(time)
osm_id <- merge(osm_id, time, by = "iris", all.x = TRUE)
osm_id$geometry <- NULL

# Number of climbing area at less than 15 minutes by bike
n15mn <- df
n15mn <- data.frame(df, stringsAsFactors = FALSE)
n15mn[n15mn < 15] <- 1
n15mn[n15mn > 15] <- 0
n15mn$N <- rowSums(n15mn)
n15mn$iris <- row.names(n15mn)
osm_id <- merge(osm_id, n15mn[,c("iris", "N")], by = "iris",
                all.x = TRUE)
osm_id <- osm_id[,c(1,3:6)]
colnames(osm_id) <- c("CODE_IRIS", "ALL_NAME", "TYPE_STRUCT", "ALL_TIME",
                      "N_15MN")
iris <- merge(iris, osm_id, by = "CODE_IRIS", all.x = TRUE)

# Prive climbing club (fees $$$)
# Name of the nearest structure
osm_id <- colnames(df2)[apply(df2, 1, which.min)] # Name
osm_id <- data.frame(osm_id, stringsAsFactors = FALSE)
osm_id$iris <- row.names(df2)
osm_id <- merge(osm_id, poi[,c("osm_id", "name", "type")], 
                by = "osm_id", all.x = TRUE)

# Time to the nearest climbing area
time <- apply(df2, 1, min) # Time
time <- data.frame(time, stringsAsFactors = FALSE)
time$iris <- row.names(time)
osm_id <- merge(osm_id, time, by = "iris", all.x = TRUE)
osm_id$geometry <- NULL

# Number of climbing area at less than 15 minutes by bike
n15mn <- df2
n15mn <- data.frame(df2, stringsAsFactors = FALSE)
n15mn[n15mn < 15] <- 1
n15mn[n15mn > 15] <- 0
n15mn$N <- rowSums(n15mn)
n15mn$iris <- row.names(n15mn)
osm_id <- merge(osm_id, n15mn[,c("iris", "N")], by = "iris",
                all.x = TRUE)
osm_id <- osm_id[,c(1,3,5:6)]
colnames(osm_id) <- c("CODE_IRIS", "PRIV_NAME", "PRIV_TIME",
                      "N_PRIV_15MN")
iris <- merge(iris, osm_id, by = "CODE_IRIS", all.x = TRUE)

# FFME associative structure
# Name of the nearest structure
osm_id <- colnames(df3)[apply(df3, 1, which.min)] # Name
osm_id <- data.frame(osm_id, stringsAsFactors = FALSE)
osm_id$iris <- row.names(df3)
osm_id <- merge(osm_id, poi[,c("osm_id", "name", "type")], 
                by = "osm_id", all.x = TRUE)

# Time to the nearest climbing area
time <- apply(df3, 1, min) # Time
time <- data.frame(time, stringsAsFactors = FALSE)
time$iris <- row.names(time)
osm_id <- merge(osm_id, time, by = "iris", all.x = TRUE)
osm_id$geometry <- NULL

# Number of climbing area at less than 15 minutes by bike
n15mn <- df3
n15mn <- data.frame(df3, stringsAsFactors = FALSE)
n15mn[n15mn < 15] <- 1
n15mn[n15mn > 15] <- 0
n15mn$N <- rowSums(n15mn)
n15mn$iris <- row.names(n15mn)
osm_id <- merge(osm_id, n15mn[,c("iris", "N")], by = "iris",
                all.x = TRUE)
osm_id <- osm_id[,c(1,3,5:6)]
colnames(osm_id) <- c("CODE_IRIS", "FFME_NAME", "FFME_TIME",
                      "N_FFME_15MN")
iris <- merge(iris, osm_id, by = "CODE_IRIS", all.x = TRUE)

# FSGT associative structure
# Name of the nearest structure
osm_id <- colnames(df4)[apply(df4, 1, which.min)] # Name
osm_id <- data.frame(osm_id, stringsAsFactors = FALSE)
osm_id$iris <- row.names(df4)
osm_id <- merge(osm_id, poi[,c("osm_id", "name", "type")], 
                by = "osm_id", all.x = TRUE)

# Time to the nearest climbing area
time <- apply(df4, 1, min) # Time
time <- data.frame(time, stringsAsFactors = FALSE)
time$iris <- row.names(time)
osm_id <- merge(osm_id, time, by = "iris", all.x = TRUE)
osm_id$geometry <- NULL

# Number of climbing area at less than 15 minutes by bike
n15mn <- df4
n15mn <- data.frame(df4, stringsAsFactors = FALSE)
n15mn[n15mn < 15] <- 1
n15mn[n15mn > 15] <- 0
n15mn$N <- rowSums(n15mn)
n15mn$iris <- row.names(n15mn)
osm_id <- merge(osm_id, n15mn[,c("iris", "N")], by = "iris",
                all.x = TRUE)
osm_id <- osm_id[,c(1,3,5:6)]
colnames(osm_id) <- c("CODE_IRIS", "FSGT_NAME", "FSGT_TIME",
                      "N_FSGT_15MN")
iris <- merge(iris, osm_id, by = "CODE_IRIS", all.x = TRUE)


# 6. Characterise the POI neighbourhood ----
t.df <- data.frame(t(df))
colnames(t.df) <- iris10k[iris10k$TYP_IRIS == "H",]$CODE_IRIS
t.df$osm_id <- dest$osm_id
t.df <- t.df[t.df$osm_id %in% poi$osm_id,]

poi_socio <- data.frame(matrix(nrow = 0, ncol = 5))
colnames(poi_socio) <- c("osm_id", "SUM_POP18", "MIN_REV19", "MOY_REV19", "MAX_REV19")

for (i in 1:nrow(t.df)){
  tmp1 <- t.df[, t.df[i, ] < 15]
  tmp2 <- data.frame(CODE_IRIS = colnames(tmp1))
  tmp2 <- merge(tmp2, iris10k[,c("CODE_IRIS", "P18_POP", "DISP_MED19")],
             all.x = TRUE)
  
  poi_socio[i,1] <- row.names(tmp1)[i]
  poi_socio[i,2] <- sum(tmp2$P18_POP, na.rm = TRUE)
  poi_socio[i,3] <- min(tmp2$DISP_MED19, na.rm = TRUE)
  poi_socio[i,4] <- mean(tmp2$DISP_MED19, na.rm = TRUE)
  poi_socio[i,5] <- max(tmp2$DISP_MED19, na.rm = TRUE)
  }

poi <- merge(poi, poi_socio, by = "osm_id", all.x = TRUE)

st_write(com, "data-conso/com.geojson")
st_write(iris, "data-conso/iris.geojson")
st_write(poi, "data-conso/poi.geojson")
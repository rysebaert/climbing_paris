## Climbing in Paris 
### Travel time calculation with R and data visualization with Observable - Application to artificial climbing walls in Paris and its neighbourhood

The aim of this repository consists in showing how to build, visualize and reproduce accessibility indicators by combining points of interest (POI) coming from the OpenStreetMap database, socio-economic indicators included in small territorial division (IRIS) coming from institutional data source (INSEE) and routing engines (OSRM). It is applied to sport climbing in Paris, but this example could be easily extended to other areas of interest or other amenities (OpenStreetMap key-values).

- To introduce the reader to the issues raised by indoor sport-climbing in Paris, have a look to [this notebook](https://observablehq.com/@rysebaert/forewords?collection=@rysebaert/climbing_paris).
- The graphical outputs displayed in this notebook are also available in an [Observable collection](https://observablehq.com/collection/@rysebaert/climbing_paris).
- The methodological framework is detailed in a [Quarto document](https://rysebaert.github.io/climbing_paris/), which combines 2 computer languages : R for data processing and Observable JavaScript for data visualizations. 

![](fig_readme.PNG)

### Usage
For further uses, core files of this repository are the following. 

- index.html: the main methodological document, distributed [here](https://rysebaert.github.io/climbing_paris/))
- [index.qmd](https://github.com/rysebaert/climbing_paris/blob/main/index.qmd): the Quarto document created to build the html file.
- [script.R](https://github.com/rysebaert/climbing_paris/blob/main/script.R): The entire R script created to build the output files used for data visualization.
- [script_docker.txt](https://github.com/rysebaert/climbing_paris/blob/main/script_docker.txt) : the script I run on my computer to run my own instance of OSRM. 
- [data-conso folder](https://github.com/rysebaert/climbing_paris/tree/main/data-conso) : the output files created from the R script, further described below.

### Data-conso content

This folder includes all the files useful to produce the data visualizations, and intermediate files useful to avoid to compute at everytimes heave calcualtions. 

#### Geojson files used in the data visualization. 

- com.geojson: municipality layer.
- iris.geojson: IRIS layer (below the municipality level), with accessibility indicators
- iris15.geojson: IRIS located at less than 15 minutes from a climbing area.
- poi: climbing areas, with some attributes.
- iso_all, iso_fsgt, iso_priv: isochrones describing the travel time by bike to reach the nearest climbing area (all areas, of the FSGT federation and for private areas).
- trip: the fastest travel to reach all climbing areas of the FSGT federation for this study area.

All these layers are in latitude / longitude (WGS84). The attributes they include are further described in the Quarto document or in an [Observable notebook](https://observablehq.com/@rysebaert/dataset-presentation-accessibility?collection=@rysebaert/climbing_paris).

### Intermediate files

- bike-duration.csv : travel time by bike between each IRIS centroid and climbing areas (all climbing areas, held by the FFME or FSGT federations only, or managed by a private brand).
- grid-bike-duration.csv : same file than above, but from a 150 m regular grid centroids. Time distance to FFME climbing areas are not calculated here.
- time_federation_iris.csv : some indicators to qualify the socio-economic neighbourhood of the climbing areas. 

### License
Attribution-ShareAlike 4.0 International (CC BY-SA 4.0)

This is a human-readable summary of (and not a substitute for) the license. Disclaimer.
You are free to:
* Share — copy and redistribute the material in any medium or format
* Adapt — remix, transform, and build upon the material for any purpose, even commercially.

This license is acceptable for Free Cultural Works. The licensor cannot revoke these freedoms as long as you follow the license term
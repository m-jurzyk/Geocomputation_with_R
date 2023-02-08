
#Geocomputation with R----

#Robin Lovelace, Jakub Nowosad, Jannes Muenchow

#2021-08-27

#To reproduce the code in the book, you need a recent version of R and up-to-date packages. 
#These can be installed with the following command (which requires remotes):

#remotes::install_github("geocompr/geocompkg")

#To build the book locally, clone or download the geocompr repo, load R in root directory
#(e.g. by opening geocompr.Rproj in RStudio) and run the following lines:

#bookdown::render_book("index.Rmd") # to build the book

#browseURL("_book/index.html") # to view i

##Chapter 1 Introduction ---- 

###1.6 Exercises ----

#1.6_a Think about the terms ‘GIS’, ‘GDS’ and ‘geocomputation’ described above. 
#Which (if any) best describes the work you would like to do using geo
#methods and software and why?


#GIS -  Geographic Information Science 

#GDS - Geographic Data science 

# Geocomputation -  usage of different geodata and developing 
#geo-tools with ‘scientific’ approach.”

# For me GDS/ Geocomputation for scientific proposes, including labour market 
# Geo statistic/ GDS proposes, but also urban climatology aspect of GIS with 
# R programming skills in the future 

#1.6_b

#Provide three reasons for using a scriptable languagesuch as R 
#for geocomputation instead of using an established GIS program such as QGIS.

# 1_ Automatisation of workflow 
# 2_ Constancy of output maps / script workflow for better understanding and sharing results
# 3_ Data oriented workflow 

#1.6_c Name two advantages and two disadvantages of using mature vs recent packages
#for geographic data analysis (for example sp vs sf).


#Advantages of using mature packages 

# Coherency and stability of output script
# Certainty of repeatbility 

#Disadvantages of using mature packages 

# Formatting/ data problems 
# New scripts can be unreadable for R 


# THE end of the chapter

##Chapter 2 Geographic data in R ----

#install.packages("sf")
#install.packages("raster")
#install.packages("spData")
#remotes::install_github("Nowosad/spDataLarge") ## Problem with installation 

library(sf)
library(raster)
library(spData)
library(spDataLarge)

###2.2.1 An introduction to simple features ---- 


names(world)

plot(world)

summary(world["lifeExp"])

###2.2.2 Why simple features? ---- 

library(sp)

world_sp = as(world, Class = "Spatial")

###2.2.3 Basic map making ----

plot(world[5:6])
plot(world["pop"])

world_asia = world[world$continent == "Asia", ]
asia = st_union(world_asia)

plot(world["pop"], reset = FALSE)
plot(asia, add = TRUE, col = "red")


###2.2.4 Base plot arguments ---- 


plot(world["continent"], reset = FALSE)
cex = sqrt(world$pop) / 10000
world_cents = st_centroid(world, of_largest = TRUE)
plot(st_geometry(world_cents), add = TRUE, cex = cex)


india = world[world$name_long == "India", ]
plot(st_geometry(india), expandBB = c(0, 0.2, 0.1, 1), col = "gray", lwd = 3)
plot(world_asia[0], add = TRUE)


###2.2.5 Geometry types - skip

###2.2.6 Simple feature geometries (sfg) ---- 

#A point: st_point()
#A linestring: st_linestring()
#A polygon: st_polygon()
#A multipoint: st_multipoint()
#A multilinestring: st_multilinestring()
#A multipolygon: st_multipolygon()
#A geometry collection: st_geometrycollection()


## 2.3 Raster data ---- 


raster_filepath = system.file("raster/srtm.tif", package = "spDataLarge")
new_raster = raster(raster_filepath)

new_raster

plot(new_raster)

spplot(new_raster)

levelplot(new_raster)

raster_filepath = system.file("raster/srtm.tif", package = "spDataLarge")
new_raster = raster(raster_filepath)

new_raster2 = raster(nrows = 6, ncols = 6, res = 0.5, 
                     xmn = -1.5, xmx = 1.5, ymn = -1.5, ymx = 1.5,
                     vals = 1:36)

## 2.4 Coordinate Reference Systems ---- 

library(rgdal)

crs_data = rgdal::make_EPSG()
View(crs_data)


vector_filepath = system.file("vector/zion.gpkg", package = "spDataLarge")
new_vector = st_read(vector_filepath)

st_crs(new_vector) # get CRS

new_vector = st_set_crs(new_vector, 4326) # set CRS

###2.6 Exercises ---- 

#1_ Use summary() on the geometry column of the world data object. 
# What does the output tell us about:

summary(world)

  #1_a Its geometry type?

# Multipolygon 

  #1_b The number of countries?

# 177?

  #1_3 Its coordinate reference system (CRS)?

# epsg:4326

#2_ Run the code that ‘generated’ the map of the world in Figure 2.5 
#at the end of Section 2.2.4. 

plot(world["continent"], reset = FALSE)
cex = sqrt(world$pop) / 10000
world_cents = st_centroid(world, of_largest = TRUE)
plot(st_geometry(world_cents), add = TRUE, cex = cex)


#Find two similarities and two differences between the image on your computer 
#and that in the book.


# Differences: 
# Line weight of circles? 
# Projection system?
# Earth grid 
# Scale 

# Similarities:

# Color palette
# Data are probably the same 


#2_a What does the cex argument do (see ?plot)?
#2_b Why was cex set to the sqrt(world$pop) / 10000?
#2_c Bonus: experiment with different ways to visualize the global population.


  



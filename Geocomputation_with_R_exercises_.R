
#Geocomputation with R----

#Robin Lovelace, Jakub Nowosad, Jannes Muenchow

#2021-08-27 - ~BOOK Publication 

#To reproduce the code in the book, you need a recent version of R and up-to-date packages. 
#These can be installed with the following command (which requires remotes):

#remotes::install_github("geocompr/geocompkg")

#To build the book locally, clone or download the geocompr repo, load R in root directory
#(e.g. by opening geocompr.Rproj in RStudio) and run the following lines:

#bookdown::render_book("index.Rmd") # to build the book

#browseURL("_book/index.html") # to view i

#Chapter 1 Introduction ---- 

##1.6 Exercises ----

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

#Chapter 2 Geographic data in R ----

##2.1 Introduction ----


#install.packages("sf")
#install.packages("raster")
#install.packages("spData")
#remotes::install_github("Nowosad/spDataLarge") ## Problem with installation 

library(sf)
library(raster)
library(spData)
library(spDataLarge)

##2.2 Vector data ---- 

# Skipped 

####2.2.1 An introduction to simple features ---- 


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

##2.6 Exercises ---- 

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
plot(st_geometry(world_cents), add = TRUE, cex=cex)


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

# Cex argument is mapping number of population, by the size of circle in the 
# middle of given country polygon.

#2_b Why was cex set to the sqrt(world$pop) / 10000?

# Hmm why? 

#2_c Bonus: experiment with different ways to visualize the global population.

#Chapter 3 Attribute data operations ----

library(sf)
library(raster)
library(dplyr)
library(stringr) # for working with strings (pattern matching)
library(tidyr)   # for unite() and separate()

library(spData)

###3.1 Introduction----

dim(world) # it is a 2 dimensional object, with rows and columns

nrow(world) # how many rows?

ncol(world) # how many columns?

###3.2 Vector attribute manipulation ----

methods(class = "sf") # methods for sf objects 

world[1:6, ] # subset rows by position
world[, 1:3] # subset columns by position
world[, c("name_long", "lifeExp")] # subset columns by name

sel_area = world$area_km2 < 10000
summary(sel_area)

small_countries = world[sel_area, ]

small_countries


small_countries = world[world$area_km2 < 10000,]

small_countries

small_countries = subset(world, area_km2 < 10000)

small_countries

## Select function from dplyr package 

world1 = dplyr::select(world, name_long, pop)
names(world1)

# all columns between name_long and pop (inclusive)
world2 = dplyr::select(world, name_long:pop)

# all columns except subregion and area_km2 (inclusive)
world3 = dplyr::select(world, -subregion, -area_km2)

#Conveniently, select() lets you subset and rename columns 
#at the same time, for example:
world4 = dplyr::select(world, name_long, population = pop)
names(world4)

world5 = world[, c("name_long", "pop")] # subset columns by name
names(world5)[names(world5) == "pop"] = "population" # rename column manually

#TABLE 3.1: Comparison operators that return Booleans (TRUE/FALSE).

#Symbol	Name
#==	Equal to
#!=	Not equal to
#>, <	Greater/Less than
#>=, <=	Greater/Less than or equal
#&, &#124;, !	Logical operators: And, Or, Not



#Using %>%  in SF 

world7 = world %>%
  filter(continent == "Asia") %>%
  dplyr::select(name_long, continent) %>%
  slice(1:5)

world8 = slice(
  dplyr::select(
    filter(world, continent == "Asia"),
    name_long, continent),
  1:5)

#### 3.2.2 Vector attribute aggregation ----

world_agg1 = aggregate(pop ~ continent, FUN = sum, data = world, na.rm = TRUE)

world_agg1 %>% ggplot(mapping = aes(x=continent, y=pop))+
  geom_col()+
  theme_minimal()

world_agg2 = aggregate(world["pop"], by = list(world$continent),
                       FUN = sum, na.rm = TRUE)

world_agg2


world_agg3 = world %>%
  group_by(continent) %>%
  summarize(pop = sum(pop, na.rm = TRUE))

world_agg3


world %>% 
  dplyr::select(pop, continent) %>% 
  group_by(continent) %>% 
  summarize(pop = sum(pop, na.rm = TRUE), n_countries = n()) %>% 
  top_n(n = 3, wt = pop) %>%
  arrange(desc(pop)) %>%
  st_drop_geometry()

####3.2.3 Vector attribute joining ----

# Join of spatial data 

world_coffee = left_join(world, coffee_data)

# left join function used to join world and coffee_data by name_long
#variable 

plot(world_coffee["coffee_production_2017"])

#In case when names do not matches we have 2 options:

#1. Rename the key variable in one of the objects so they match.

coffee_renamed = rename(coffee_data, nm = name_long)

#2. Use the by argument to specify the joining variables.

world_coffee2 = left_join(world, coffee_renamed, by = c(name_long = "nm"))

## But in this case we have a lot of NA values for countries without coffee production

# to prevent this happening we can use inner_join function 

world_coffee_inner = inner_join(world, coffee_data)

## 2 missiing countries - to idenfity this countires we can use 

setdiff(coffee_data$name_long, world$name_long)

str_subset(world$name_long, "Dem*.+Congo")

## To fix it we will use:

coffee_data$name_long[grepl("Congo,", coffee_data$name_long)] = 
  str_subset(world$name_long, "Dem*.+Congo")

world_coffee_match = inner_join(world, coffee_data)


####3.2.4 Creating attributes and removing spatial information ----

world_new = world # do not overwrite our original data

#adding new column based on existing data

world_new$pop_dens = world_new$pop / world_new$area_km2

# mutate function:

world %>% 
  mutate(pop_dens = pop / area_km2)

#Rename colum:

world %>% 
  rename(name = name_long)

###3.3 Manipulating raster objects ----

elev = raster(nrows = 6, ncols = 6, res = 0.5,
              xmn = -1.5, xmx = 1.5, ymn = -1.5, ymx = 1.5,
              vals = 1:36)

grain_order = c("clay", "silt", "sand")
grain_char = sample(grain_order, 36, replace = TRUE)
grain_fact = factor(grain_char, levels = grain_order)
grain = raster(nrows = 6, ncols = 6, res = 0.5, 
               xmn = -1.5, xmx = 1.5, ymn = -1.5, ymx = 1.5,
               vals = grain_fact)

factorValues(grain, grain[c(1, 11, 35)]

####3.3.1 Raster subsetting ----

r_stack = stack(elev, grain)
names(r_stack) = c("elev", "grain")
# three ways to extract a layer of a stack
raster::subset(r_stack, "elev")
r_stack[["elev"]]
r_stack$elev

r_stack

####3.3.2 Summarizing raster objects----

cellStats(elev, sd)
hist(elev)


###3.4 Exercises ----

library(spData)
library(dplyr)
data(us_states)
data(us_states_df)

#_1a Create a new object called us_states_name that contains only the NAME
#column from the us_states object. 
#What is the class of the new object and what makes it geographic?             


us_states_name <- us_states$NAME

us_states_name %>% class()

#It's character object and it's not geographic, we can make it by joining data 
#with geographic database by name 

#_2a Select columns from the us_states object which contain population data. 
#Obtain the same result using a different command

us_states %>% select(
  total_pop_10, total_pop_15)

us_states %>% select(contains("pop"))


#_3Find all states with the following characteristics (bonus find and plot them):

#_3a Belong to the Midwest region.

us_states %>% 
  filter(REGION=="Midwest") %>% 
  plot()


#_3b Belong to the West region, have an area below 250,000 km2 
#and in 2015 a population greater than 5,000,000 residents 

us_states %>% glimpse()

# To check datatype

us_states %>% 
  mutate(us_states, area1=as.numeric(us_states$AREA)) %>% 
  filter(REGION=="West",area_1<250000,total_pop_15>5000000) %>% 
  plot()
  
#####QUESTION: How can I plot only one selected state with all states? ----


#_3c Belong to the South region, had an area larger than 150,000 km2 or a total 
#population in 2015 larger than 7,000,000 residents.

us_states %>% 
  mutate(us_states, area1=as.numeric(us_states$AREA)) %>% 
  filter(REGION=="South",area_1>150000,total_pop_15>7000000) %>% 
  plot()


#_4a What was the total population in 2015 in the us_states dataset?
#What was the minimum and maximum total population in 2015?

us_states %>% mutate(sum= sum(total_pop_15))

#314375347 

min(us_states$total_pop_15)

#579679

max(us_states$total_pop_15)

#38421464
  

#_5a How many states are there in each region?


us_states %>% 
  dplyr::select(NAME, REGION) %>% 
  group_by(REGION) %>% 
  summarize(n_cities= n())

  
#_6a What was the minimum and maximum total population in 2015 in each region?


p1 <- us_states %>% 
  dplyr::select(NAME, REGION, total_pop_15) %>% 
  group_by(REGION) %>% 
  summarize(total_pop_15 = sum(total_pop_15), n_citiesinregion = n()) %>% 
  arrange(desc(total_pop_15))

p1


min(p1$total_pop_15)
  
#55989520

max(p1$total_pop_15)

#118575377


#_6b What was the total population in 2015 in each region?

us_states %>% 
  dplyr::select(NAME, REGION, total_pop_15) %>% 
  group_by(REGION) %>% 
  summarize(total_pop_15 = sum(total_pop_15), n_citiesinregion = n()) %>% 
  arrange(desc(total_pop_15))

#_7a Add variables from us_states_df to us_states, and create a new object 
#called us_states_stats. What function did you use and why?

x1 <- us_states_df %>% mutate(NAME=state)

us_states

x2 <- us_states %>% left_join(x1)

#function left join and mutate, it's fairly easy

#_7b #Which variable is the key in both datasets?

# STATE NAME 

#_7c What is the class of the new object?

x2 %>% glimpse()

# It's numeric <dbl>

#_8a us_states_df has two more rows than us_states. How can you find them? 

#(hint: try to use the dplyr::anti_join() function)

us_states %>% dplyr::anti_join(x1)

nrow(us_states)

nrow(us_states_df)

#####QUESTION: Stil don't know which rows? ----


#_8b What was the population density in 2015 in each state? 

pd1 <- us_states %>% 
  mutate(us_states,
         areanum=as.numeric(us_states$AREA),
         pop_dens_15 = total_pop_15/areanum) %>% 
 select(NAME, pop_dens_15)

pd1 %>% arrange(desc(pop_dens_15))

#_9a What was the population density in 2010 in each state?

pd2 <- us_states %>% 
  mutate(us_states,
         areanum=as.numeric(us_states$AREA),
         pop_dens_10 = total_pop_10/areanum) %>% 
  select(NAME, pop_dens_10)

pd2 %>% arrange(desc(pop_dens_10))

#_10a How much has population density changed between 2010 and 2015 in each state? 
#Calculate the change in percentages and map them.

pd3 <- us_states %>% 
  mutate(us_states,
         areanum=as.numeric(us_states$AREA),
         pop_dens_10 = total_pop_10/areanum,
         pop_dens_15 = total_pop_15/areanum,
         change_pop = pop_dens_15-pop_dens_10)

# My calculations 


us_popdens_change = pd3 |>
  mutate(pop_dens_diff_10_15 = pop_dens_15 - pop_dens_10,
         pop_dens_diff_10_15p = (pop_dens_diff_10_15/pop_dens_15) * 100)
plot(us_popdens_change["pop_dens_diff_10_15p"])

#Calculations + plot from solution book

#_11a Change the columns’ names in us_states to lowercase.
#(Hint: helper functions - tolower() and colnames() may help.)

us_states

? tolower

us_states %>%
  setNames(tolower(colnames(.)))


#_12a Using us_states and us_states_df create a new object called us_states_sel. 
#he new object should have only two variables - median_income_15 and geometry.
#Change the name of the median_income_15 column to Income.

u1 <-us_states_df %>% mutate(NAME= state)

u <- us_states_sel <- left_join(us_states, u1)

u2 <- us_states_sel %>% select(median_income_15, geometry)

u3 <- u2 %>% mutate(Income=median_income_15 ) %>% 
  select(Income, geometry)

## Perhaps it's more elegant way of doing it


#_13a Calculate the change in the number of residents living below the poverty 
#level between 2010 and 2015 for each state. 
#(Hint: See ?us_states_df for documentation on the poverty level columns.) 
#Bonus: Calculate the change in the percentage of residents living below the poverty level in each state.


?us_states_df


u_13 <- u %>% select(!state)

up <- u_13 %>% mutate(
  poverty_change=poverty_level_15-poverty_level_10,
  poverty_change_share_inpercent=(poverty_change/total_pop_15)*100) %>% 
  arrange(desc(poverty_change_share_inpercent))

up

#_14a What was the minimum, average and maximum state’s number of people living 
#below the poverty line in 2015 for each region?

up1 <- up %>% 
  dplyr::select(NAME, REGION, total_pop_15, poverty_level_15, ) %>% 
  group_by(REGION) %>% 
  summarize(poverty_level_15 = sum(poverty_level_15), n_cities_in_region = n())

min(up1$poverty_level_15)

# 7240189

max(up1$poverty_level_15)

# 19508771

#Bonus: What is the region with  the largest increase in people living below the poverty line?


up3 <- up %>% 
  dplyr::select(NAME, REGION, total_pop_15, poverty_level_15,poverty_change ) %>% 
  group_by(REGION) %>% 
  summarize(poverty_change = sum(poverty_change), n_cities_in_region = n())

max(up3$poverty_change)

#SOUTH!  



#_15a Create a raster from scratch with nine rows and columns and a resolution of 0.5 decimal degrees (WGS84). 
#Fill it with random numbers. Extract the values of the four corner cells.

library(raster)

r = rast(nrow = 9, ncol = 9, res = 0.5,
         xmin = 0, xmax = 4.5, ymin = 0, ymax = 4.5,
         vals = rnorm(81))


plot(r)

#_16a What is the most common class of our example raster grain?


boxplot(r)

summary(r)

hist(r)

?global

?modal

modal(r)



#_17a Plot the histogram and the boxplot of the dem.tif file from the spDataLarge 
#package (system.file("raster/dem.tif", package = "spDataLarge"))

ra <- rast(system.file("raster/dem.tif", package = "spDataLarge"))

plot(ra)

hist(ra)



#Chapter 4 Spatial data operations ----

library(sf)
library(terra)
library(dplyr)
library(spData)

elev = rast(system.file("raster/elev.tif", package = "spData"))
grain = rast(system.file("raster/grain.tif", package = "spData"))


#RAST FUNCTION IS NOT WORKING EHHH 


### 4.1 Introduction ----

library(fuzzyjoin)
library(terra)

### 4.2 Spatial operations on vector data----

canterbury = nz |> filter(Name == "Canterbury")
canterbury_height = nz_height[canterbury, ]

#### 4.2.2 Topological relations ----

# create a polygon
a_poly = st_polygon(list(rbind(c(-1, -1), c(1, -1), c(1, 1), c(-1, -1))))
a = st_sfc(a_poly)

# create a line
l_line = st_linestring(x = matrix(c(-1, -1, -0.5, 1), ncol = 2))
l = st_sfc(l_line)

# create points
p_matrix = matrix(c(0.5, 1, -1, 0, 0, 1, 0.5, 1), ncol = 2)
p_multi = st_multipoint(x = p_matrix)
p = st_cast(st_sfc(p_multi), "POINT")

plot(a)

#####QUESTION: how to make this plot? FIGURE 4.2 ----

## Relations between polygon, line and points
st_intersects(p, a)


# Oposite of intersects 
st_disjoint(p, a, sparse = FALSE)[, 1] #(note [, 1] converts the result into a vector)


## Within the object? * only object 100% within object

st_within(p, a, sparse = FALSE)[, 1]

st_touches(p, a, sparse = FALSE)[, 1]

sel = st_is_within_distance(p, a, dist = 0.9) # can only return a sparse matrix
sel

lengths(sel) > 0

####4.2.3 Spatial Joins ----

set.seed(2018) # set seed for reproducibility
(bb_world = st_bbox(world)) # the world's bounds
#>   xmin   ymin   xmax   ymax 
#> -180.0  -89.9  180.0   83.6
random_df = tibble(
  x = runif(n = 10, min = bb_world[1], max = bb_world[3]),
  y = runif(n = 10, min = bb_world[2], max = bb_world[4])
)
random_points = random_df %>% 
  st_as_sf(coords = c("x", "y")) %>% # set coordinates
  st_set_crs(4326) # set geographic CRS


world_random = world[random_points, ]
nrow(world_random)
#> [1] 4
random_joined = st_join(random_points, world["name_long"])

plot(random_joined)

#####QUESTION: how to plot this countries? FIGURE 4.3 ----



#### 4.2.4 Non-overlapping joins ----

plot(st_geometry(cycle_hire), col = "blue")
plot(st_geometry(cycle_hire_osm), add = TRUE, pch = 3, col = "red")

# Distance 20 and projection change 

cycle_hire_P = st_transform(cycle_hire, 27700)
cycle_hire_osm_P = st_transform(cycle_hire_osm, 27700)
sel = st_is_within_distance(cycle_hire_P, cycle_hire_osm_P, dist = 20)
summary(lengths(sel) > 0)


# FALSE 304     TRUE  438 #This shows that there are 438 points in the
#target object cycle_hire_P within the threshold distance of cycle_hire_osm_P
# distance argument set to below 20m 

z = st_join(cycle_hire_P, cycle_hire_osm_P,
            join = st_is_within_distance, dist = 20)

nrow(cycle_hire)

nrow(z)

z = z %>% 
  group_by(id) %>% 
  summarize(capacity = mean(capacity))
nrow(z) == nrow(cycle_hire)

plot(cycle_hire_osm["capacity"])
plot(z["capacity"])


#### 4.2.5 Spatial data aggregation ----

nz_avheight = aggregate(x = nz_height, by = nz, FUN = mean)

nz_avheight2 = nz %>%
  st_join(nz_height) %>%
  group_by(Name) %>%
  summarize(elevation = mean(elevation, na.rm = TRUE))

agg_aw = st_interpolate_aw(incongruent[, "value"], aggregating_zones,
                           extensive = TRUE)
#> Warning in st_interpolate_aw.sf(incongruent[, "value"], aggregating_zones, :
#> st_interpolate_aw assumes attributes are constant or uniform over areas of x
# show the aggregated result
agg_aw$value

#### 4.2.6 Distance relations----

nz_heighest = nz_height %>% top_n(n = 1, wt = elevation)
canterbury_centroid = st_centroid(canterbury)
st_distance(nz_heighest, canterbury_centroid)

# it is returned as a matrix in meters unit 

co = filter(nz, grepl("Canter|Otag", Name))
st_distance(nz_height[1:3, ], co)

plot(st_geometry(co)[2])
plot(st_geometry(nz_height)[2:3], add = TRUE)


##4.3 Spatial operations on raster data----

# elev and grain can be found in spData package

#### 4.3.1 Spatial subsetting----

id = cellFromXY(elev, xy = c(0.1, 0.1))
elev[id]
# the same as
raster::extract(elev, data.frame(x = 0.1, y = 0.1))

#clip 

clip = raster(xmn = 0.9, xmx = 1.8, ymn = -0.45, ymx = 0.45,
              res = 0.3, vals = rep(1, 9))
elev[clip]

elev[1:2, drop = FALSE]    # spatial subsetting with cell IDs
elev[1, 1:2, drop = FALSE] # spatial subsetting by row,column indices



# create raster mask
rmask = elev 
values(rmask) = sample(c(NA, TRUE), 36, replace = TRUE)

# spatial subsetting
elev[rmask, drop = FALSE]           # with [ operator
mask(elev, rmask)                   # with mask()
overlay(elev, rmask, fun = "max")   # with overlay

#### 4.3.2 Map algebra----

# Theory about map algebra 

#### 4.3.3 Local operations ----


rcl = matrix(c(0, 12, 1, 12, 24, 2, 24, 36, 3), ncol = 3, byrow = TRUE)
recl = reclassify(elev, rcl = rcl)

# Included in RASTER package 

elev + elev
elev^2
log(elev)
elev > 5

## calc() - overlay() functions in raster 

# NDVI calculations # Satellite data raster calculations 


# B4 path "/Users/maciejjurzyk/Downloads/S2B_MSIL1C_20230205T111139_N0509_R137_T29SQB_20230205T115207.SAFE/GRANULE/L1C_T29SQB_A030908_20230205T111142/IMG_DATA/T29SQB_20230205T111139_B04.jp2"

# B8 path "/Users/maciejjurzyk/Downloads/S2B_MSIL1C_20230205T111139_N0509_R137_T29SQB_20230205T115207.SAFE/GRANULE/L1C_T29SQB_A030908_20230205T111142/IMG_DATA/T29SQB_20230205T111139_B08.jp2"


red <- rast("/Users/maciejjurzyk/Downloads/S2B_MSIL1C_20230205T111139_N0509_R137_T29SQB_20230205T115207.SAFE/GRANULE/L1C_T29SQB_A030908_20230205T111142/IMG_DATA/T29SQB_20230205T111139_B04.jp2")

nir <- rast("/Users/maciejjurzyk/Downloads/S2B_MSIL1C_20230205T111139_N0509_R137_T29SQB_20230205T115207.SAFE/GRANULE/L1C_T29SQB_A030908_20230205T111142/IMG_DATA/T29SQB_20230205T111139_B08.jp2")

ndvi = (nir - red) / (nir + red)

plot(ndvi)

#### 4.3.4 Focal operations ----

# focal function to spatial filetering 

r_focal = focal(elev, w = matrix(1, nrow = 3, ncol = 3), fun = min)


#### 4.3.5 Zonal operations ----

z = zonal(elev, grain, fun = "mean") %>%
  as.data.frame()

z


#### 4.3.6 Global operations and distances ----

#### 4.3.7 Map algebra counterparts in vector processing ----

#### 4.3.8 Merging rasters ----

aut = getData("alt", country = "AUT", mask = TRUE)
ch = getData("alt", country = "CHE", mask = TRUE)
aut_ch = merge(aut, ch)

### 4.4 Exercises  ====

#_1a It was established in Section 4.2 that Canterbury was the region of New Zealand 
#containing most of the 100 highest points in the country.
#How many of these high points does the Canterbury region contain?

library(tmap)

qtm(nz)+qtm(nz_height)

canterbury <-  nz %>% filter(Name=="Canterbury")

canterbury_height <- nz_height[canterbury, ]

nz_not_cantenbury <- nz_height[canterbury, , op = st_disjoint]

nrow(canterbury_height)

plot(st_geometry(nz))

plot(st_geometry(canterbury), col="yellow", add= TRUE)

plot(nz_not_cantenbury$geometry, pch=1, col="blue", add=TRUE)

plot(canterbury_height$geometry, pch=1, col="red", add= TRUE)

#_2a Which region has the second highest number of nz_height points in, 
#and how many does it have?

nz_height_count = aggregate(nz_height, nz, length)

nz_height_combined = cbind(nz,count = nz_height_count$elevation)

nz_height_combined %>% 
  st_drop_geometry() %>% 
  select(Name, count) %>% 
  arrange(desc(count)) %>% 
  slice(2)


#_3a Generalizing the question to all regions:
#how many of New Zealand’s 16 regions contain points 
#which belong to the top 100 highest points in the country? Which regions?

#Bonus: create a table listing these regions in order
#of the number of points and their name.

nz_height_joint <- 
  st_join(nz_height, nz) %>% 
  select(Name)

nz_height_count <- nz_height_joint %>% 
  group_by(Name) %>% 
  summarise(count=n())

nz_height_count


#_4a Test your knowledge of spatial predicates by finding out and plotting
#how US states relate to each other and other spatial objects.
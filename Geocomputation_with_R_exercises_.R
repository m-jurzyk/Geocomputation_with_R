
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
#remotes::install_github("Nowosad/spDataLarge") ## Problem with instalation 

library(sf)
library(raster)
library(spData)
library(spDataLarge)



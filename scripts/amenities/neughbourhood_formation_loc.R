#### Libraries ####

library(sf)
library(tidyverse)
library(osmdata)
library(ggplot2)
library(here)
library(tmap)
library(tmaptools)
library(igraph)
library(doParallel)
library(stplanr)
# performance monitoring
library(profvis)
library(bench)
library(pryp)
library(concaveman)
library(osmdata)

#### memory usage. ####

gc() #This will free up unused RAM
gcinfo(TRUE)

mem_used()

#### FUNCTIONS ####
source("/Users/ivannschlosser/Documents/dissertation_RC/supp/functions.R")

#### Data ####
 
# # data set of amenities with the entropy and size variables.
# london_entropy <- sf::st_read(dsn = here::here("london_entropy.geojson"))


#### Marylebone example of clustering ####

marylebone <- neighbourhoods |> filter(neighbourhood == "Marylebone") |>
  st_set_crs(4326) |>
  st_transform(27700) |>
  st_buffer(dist = 1000)

# marylebone_road_2 <- getOSMdata(sf::st_bbox(marylebone),k='highway',val='residential')

tmap_mode("view")
marylebone |> qtm()

london_amenities_isochrones$id <- as.character(london_amenities$osm_id)
entropy_iso$osm_id <- as.character(london_entropy$osm_id)

marylebone_neighbourhood <- entropy_iso |> 
  filter(st_intersects(geometry,marylebone, sparse = FALSE))

marylebone_roads <- london_roads |> 
  st_transform(27700) |> 
  filter(st_intersects(geometry,marylebone,sparse = FALSE))

marylebone_isochrones <- london_amenities_isochrones |> 
  filter(id %in% marylebone_neighbourhood$osm_id) |>
  st_set_crs(4326) |> 
  st_transform(27700)

marylebone_neighbourhood |> qtm(dots.col = "entropy"
                                 )

marylebone_neighbourhood |> filter(osm_id == "802152485")

int <- sf::st_intersects(marylebone_isochrones, marylebone_neighbourhood)
# once we have made the intersection, we need to check that they all intersect at least with their own amenity
# this is not the case every time because some amenities are in locations with no roads around
# while the isochrones uses the underlyinig road network to build the areas. 
marylebone_neighbourhood <- marylebone_neighbourhood |> 
  dplyr::mutate(ind = 1:nrow(marylebone_neighbourhood))
i <- 1294
nb <- marylebone_neighbourhood[int[[i]],]
indice <- i
glob_ind <- c(i)
#osmid <- data$osm_id[i]
m <- nb$ind[which(max(nb$entropy) == nb$entropy)[1]]
while(m != indice) {
  nb <- marylebone_neighbourhood[int[[m]],]
  indice <- m
  glob_ind <- c(glob_ind,m)
  m <- nb$ind[which(max(nb$entropy) == nb$entropy)[1]]
}
glob_ind

tmap_mode("plot")

marylebone_neighbourhood$steps <- "omitted"

marylebone_neighbourhood$steps[glob_ind] <- paste(c("start", "intermediate max","intermediate max", "local max")
                                                  ,", S="
                                                  ,round(marylebone_neighbourhood$entropy[glob_ind],2)
                                                  ,sep = "")

marylebone_clustering_map <- tm_shape(marylebone_roads
                                      ,bbox = marylebone) + tm_lines(col = "black"
                                      ,alpha = 1) +
  tm_shape(marylebone_neighbourhood) + tm_dots(col = "entropy"
                                               ,palette = "viridis"
                                               ,style = "fisher"
                                               ,size = 0.05
                                               ,breaks = c(.5,1,1.5,2,2.5,3,3.2,3.5)
                                               ) + 
  tm_shape(marylebone_isochrones[glob_ind,]) + tm_polygons(col = "grey"
                                                           ,alpha = 0.2) +
  tm_shape(marylebone_neighbourhood[glob_ind,]) + tm_dots(col = "red") + 
  tm_text(text = "steps"
          ,ymod = 1) + tm_scale_bar(breaks = c(0,0.5,1))

marylebone_clustering_map

tmap_save(marylebone_clustering_map
          ,"marylebone_clustering_map.pdf"
          ,height = h
          ,width = w)


#### neighbourhoods with buffers (NOT USED IN FINAL WORK)####

# neighbourhood_buff <- read_csv("neighbourhood_buff2/clus_buff.csv")
# 
# neighbourhood_buff
# 
# colnames(neighbourhood_buff) <- c("buff500","buff1000","buff1500")
# 
# london_entropy_clus <- london_entropy |> bind_cols(neighbourhood_buff)
# 
# tmap_mode("plot")
# 
# neighbourhoods_1000 <- tm_shape(london_roads) + tm_lines(col = "black") + 
#   tm_shape(london_entropy_clus) + tm_dots(col = "buff1000"
#                                      ,palette = "Set1"
#                                      ,alpha = 0.5
#                                      ,size = 0.01
#                                      ) + 
#   tm_layout(legend.show = FALSE
#             ,main.title = "Neighbourhoods"
#             ,title = "Buffers, r = 1000m") + tm_scale_bar()
# 
# neighbourhoods_500
# 
# tmap_save(neighbourhoods_1000,"neighbourhoods_buff_500.pdf") 


#### neighbourhoods with isochrones ####

neighbourhood_iso <- readr::read_csv("neighbourhood_iso/max_iso.csv") |> 
  dplyr::rename("cluss_iso" = ".")

london_amenities <- london_entropy # |> dplyr::select(osm_id,name,amenity)
london_amenities <- london_amenities |> dplyr::bind_cols(neighbourhood_iso)


tmap_mode("plot")
neighbourhoods_iso_map <- tm_shape(london_roads, bbox = westminsterbb) + tm_lines(col = "black") + 
  tm_shape(london_amenities) + tm_dots(col = "cluss_iso"
                                      # ,style = "cat"
                                      ,palette = "Set1"
                                      ,alpha = 0.5
                                      ,size = 0.01) + 
  tm_layout(legend.show = FALSE
            ,main.title = "Neighbourhoods"
            ,title = "isochrones t = 7 min") + tm_scale_bar()
neighbourhoods_iso_map

tmap_save(neighbourhoods_iso_map
          ,"neighbourhoods_iso_map.pdf"
          ,height = h
          ,width = w) 

tmap_mode("view")

london_amenities$cluss_iso |> unique()

# selected_cluss <- london_amenities |>
#   sf::st_drop_geometry() |> 
#   dplyr::group_by(cluss_iso) |> 
#   count() |> 
#   arrange(n) |> 
#   tail() |> 
#   pull(cluss_iso)
# 
# 
# london_amenities |>
#   filter(cluss_iso %in% selected_cluss) |>
#   concaveman::concaveman(concavity = 2.5) |>
#   qtm(
#     # dots.col='entropy'
#     #   ,paelette='viridis'
#       )
# 
# sel_cluss <- data.frame(cluss_iso=NULL,geometry=NULL)
# for(i in selected_cluss){
#   sel_cluss <- rbind.data.frame(sel_cluss
#                                 ,london_amenities |>
#                                   filter(cluss_iso == i) |>
#                                   concaveman::concaveman(concavity = 2.5))
# }
# 
# sel_cluss |> qtm()

## Example of clustering procedure. 

london_amenities_cluss <- london_amenities |> 
  group_by(cluss_iso) |> 
  mutate(size=n()) |> 
  filter(size>5)

cluss_hulls <- data.frame(cluss_iso=NULL,geometry=NULL)
for(i in unique(london_amenities_cluss$cluss_iso)){
  cluss_hulls <- rbind.data.frame(sel_cluss
                                ,london_amenities |>
                                  filter(cluss_iso == i) |>
                                  concaveman::concaveman(concavity = 2.5))
}

leaflet::leaflet() |> 
  leaflet::addTiles() |> 
  leafgl::addGlPolygons(cluss_hulls |> sf::st_transform(4326)
                        # ,color = 'black'
                        ,fillOpacity = .6
                        ,fillColor = 'dimgray')

qtm(cluss_hulls,fill.alpha=.2)

# combined amenities by neighbourhood with count
# 
# amenities_combined <- london_amenities |> 
#   group_by(cluss_iso) |> 
#   summarize(combined = st_combine(geometry), number = n()) |> 
#   st_centroid()
# 
# neighbourhoods from concave hulls enveloping the amenities of a single neighbourhood
# neighbourhoods_iso_hull <-
#   london_amenities |> 
#   group_by(cluss_iso) |> 
#   mutate(size=n()) |> 
#   filter(size>3) |> 
#   group_modify(.f=concaveman::concaveman,concavity = 2.5) |> 
#   rename(geometry = polygons) |> 
#   sf::st_sf() 

neighbourhoods_iso_hull <- cluss_hulls |> 
  rename(geometry=polygons)

south_londonbb <- sf::st_read("GPS signals part/south_londonbb.geojson") |>
  st_set_crs(27700)

neighbourhoods_iso_hull |>
  filter(st_intersects(geometry,south_londonbb, sparse = FALSE)) |> 
  qtm(fill = "MAP_COLORS",fill.alpha=.6, bbox = south_londonbb |> st_buffer(dist = -1500))


neighbourhoods_iso_hull |> sf::st_write("neighbourhoods_iso_hull.geojson")

# westminster area 
westminster_iso_hull <- neighbourhoods_iso_hull |>
  filter(st_intersects(geometry,westminsterbb, sparse = FALSE))

westminster_iso_hull |> qtm(fill = "MAP_COLORS",fill.alpha=.6)
  
sf::st_write(westminster_iso_hull,"westminster_iso_hull.geojson") 

#### Map making ####

# neighbourhood concave hulls
london_neighbourhoods_artsy <- tm_shape(london_roads) + tm_lines(col="black"
                                  ,alpha = 0.6
                           ) +
  tm_shape(neighbourhoods_iso_hull) + tm_fill(col= "MAP_COLORS"
                                              ,palette = "Set1"
                                              ,alpha = .8
                                              ,style = "cat"
                                              ) +
  tm_layout(legend.show = FALSE
            ,title = "Neighbourhoods") +
  tm_scale_bar()
london_neighbourhoods_artsy

tmap_save(london_neighbourhoods_artsy
          ,"neighbourhoods_iso_pretty.pdf"
          ,height = h
          ,width = w)

##  numer of amenities in neighbourhoods as dots size
amenities_combined_map <- tm_shape(thames) + tm_lines(col = "darkblue") +
  tm_shape(parks) + tm_polygons(col = "green") +
  tm_shape(amenities_combined |> filter(number>5)) + tm_dots(size = "number"
                                         ,legend.size.is.portrait = TRUE
                                         
                                         
  ) +
  tm_layout(main.title = "Neighbouhoods"
            #,legend.height = 1.5
  )
amenities_combined_map

tmap_save(amenities_combined_map
          ,"neighbourhoods_amenities_combined.pdf"
          ,height = h
          ,width = w)


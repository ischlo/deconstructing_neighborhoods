library(sf)
library(tidyverse)
library(data.table)
library(here)
library(rlist)
library(concaveman)
library(cppRouting)
library(tmap)
library(tmaptools)
library(foreach)
library(doParallel)
library(sfarrow)
library(plotly)
library(Btoolkit)

options(max.print = 50
        ,shiny.autoreload = TRUE)

gc()

source("/Users/ivannschlosser/Documents/CASA/scripts/network/cppr_network_setup.R")

## data ## 

# osm all from quant active travel
road_network <- list.load("/Users/ivannschlosser/Documents/CASA/benchmarks/cppr_networks/osm_all.rds") 

amenities <- st_read("data/london_amenities.geojson") 

# grid_short <- 
#   rlist::list.load('/Users/ivannschlosser/Documents/Dissertation/Code & Data/GPS signals part/voronoi_hex_short/voronoi_hex_short.rdata') |> 
#   sf::st_set_crs(27700)

grid <- st_read("data/voronoi_hex_london.geojson") |> st_transform(4326)

grid <- grid |> mutate(centroid = st_centroid(geometry)) |> data.table::as.data.table()

grid$id <- 1:nrow(grid)

# nodes <- find_nearest_node_on_graph(road_network
#                                     ,grid[,"centroid"][[1]][1:10]
#                                     )

#### 

unique(amenities$amenity)

# filter out some amenities: artwork, recycling,
amenities <- amenities |> dplyr::filter(!amenity %in% c("recycling", "artwork"))

pred <- st_intersects(road_network$coords |> st_as_sf(coords = c("x","y"), crs = 4326)
                      ,grid$geometry)

nodes <- road_network$coords

nodes$intersects <- pred |> as.numeric()

nodes$intersects |> summary()

node_ids_per_hex <- nodes |>
  drop_na(intersects) |>
  group_by(intersects) |>
  summarise(node_ids = list(osmid))

node_ids_per_hex <- grid[,"id"]  |>
  left_join(node_ids_per_hex
             ,by = c("id"="intersects")) |>
  as.data.table()

# test 
# node_ids_per_hex$node_ids |> sapply(length) |> tail(100)
# 
# x <- node_ids_per_hex$node_ids[1]
# x |> length()
# 
# nodes[1:100] |>
#   drop_na(intersects) |>
#   group_by(intersects) |>
#   mutate(nearest_node = st_nearest_feature(grid[intersects,centroid]
#                                            ,geometry))

nodes <- nodes |> st_as_sf(coords = c("x","y"), crs = 4326) |> as.data.table()

# The next operatino is done with the mcmapply function which is QUICK!
# registerDoParallel(cores = 6)
# selected <- foreach(i=node_ids_per_hex$id
#                     ,.combine = c) %dopar% {
#                       if (length(node_ids_per_hex$node_ids[[i]]) != 0) nodes[intersects == i,][st_nearest_feature(grid[i,centroid]
#                                                                                                                   ,nodes[intersects == i,geometry]),osmid] 
#                       else "NA"
#                     }
# stopImplicitCluster()

selected <- parallel::mcmapply(node_ids_per_hex$id
                               ,node_ids_per_hex$node_ids
                               ,mc.cores = 6
                               ,SIMPLIFY = TRUE
                               ,FUN = \(i,val) {
                                 if (length(val) != 0) nodes[intersects == i,][sf::st_nearest_feature(grid[i,centroid]
                                                                                                  ,nodes[intersects == i,geometry]),osmid] 
                                 else "NA"
                               })
# if simplify is true no need for that
# selected <- unlist(selected)

node_ids_per_hex$selected <- selected

#### Isochrones and concave hulls ####
# stopped here

registerDoParallel(cores = 6)
isochrone_nodes <- cppRouting::get_isochrone(road_network
                                             ,from = node_ids_per_hex[selected != "NA",selected]
                                             ,lim = 500
                                             ,long = FALSE)
stopImplicitCluster()


#  alternative version below, but seems a bit slower
n_cores <- 6
registerDoParallel(cores = n_cores)
k <- trunc(length(isochrone_nodes)/n_cores)

res1 <-
  foreach(i = 1:n_cores
          ,.combine = c
          ) %dopar% {
            sapply(isochrone_nodes[((i-1)*k+1):(i*k)]
                   ,function(x) {
                     nodes[match(x,nodes$osmid), ] |>
                       st_as_sf() |>
                       concaveman() |>
                       st_geometry()
                   })
          }
stopImplicitCluster()

if(k*n_cores != length(isochrone_nodes)) {
  res2 <- sapply(isochrone_nodes[(k*n_cores+1):length(isochrone_nodes)]
                 ,function(x) {
                   nodes[match(x,nodes$node_id),] |>
                     st_as_sf() |>
                     concaveman()  |>
                     st_geometry()
                 })
} else {res2 <- NULL}

isochrone_poly <- append(res1,res2) |>
  st_sfc(crs = 4326) |> 
  data.table::as.data.table()

isochrone_poly$id <- node_ids_per_hex[selected != "NA",id]

# another way using mcmapply, possibly faster ?
# isochrone_poly <- parallel::mcmapply(isochrone_nodes
#                                      ,SIMPLIFY = TRUE
#                                      ,mc.cores = 6
#                                      ,FUN = \(x) {
#                                        nodes[match(x,nodes$osmid),geometry] |>
#                                          st_as_sf() |>
#                                          concaveman() |>
#                                          st_geometry()
#                                      }) |>
#   st_sfc(crs = 4326)

# tmap::tmap_mode('view')
# tmap::tmap_options(check.and.fix = TRUE)
# isochrone_poly[!st_is_valid(isochrone_poly)] |> tmap::qtm()

isochrone_poly[!sf::st_is_valid(geometry),'geometry'] <- 
  isochrone_poly[!sf::st_is_valid(geometry),geometry] |>
  st_make_valid()

# isochrone_poly <- isochrone_poly |> st_as_sf() |> data.table::as.data.table()

# isochrone_poly$geometry |> sf::st_is_empty()

# |> 
#   data.table::as.data.table()

# isochrone_poly[sf::st_is_empty(geometry),]

### preview

isochrone_poly$geometry |> st_as_sf() |> sf::st_is_valid() |> summary()

isochrone_poly <- isochrone_poly |> st_make_valid()

isochrone_poly <- isochrone_poly |> dplyr::filter(!sf::st_is_empty(geometry))

isochrone_poly <- isochrone_poly |> 
  sf::st_cast('POLYGON',do_split=FALSE) 

leaflet::leaflet() |> 
  leaflet::addTiles() |> 
  leafgl::addGlPolygons(isochrone_poly
                        ,color = 'black'
                        ,fillColor = 'dimgray'
                        ,fillOpacity = 1
                        )

divide <- \(x,y) return(x/y)

isochrone_poly$area_hec <-
  isochrone_poly |> 
  sf::st_area() |> 
  units::drop_units() |> 
  divide(10000) |> 
  round(3)

####

# isochrone_poly |> st_as_sf() |> st_write("app/data/isochrones_poly.geojson")

pred_amen <- st_intersects(isochrone_poly,amenities[,geometry])

#### Computing entropy ####

entropy <- mclapply(pred_amen
                    ,mc.cores = 6
                    ,FUN = function(x) {
                      if(!is_empty(x)){
                        counted <- amenities[x,] |> dplyr::group_by(amenity) |> dplyr::count()
                        p <- counted$n/sum(counted$n)
                        e <- c(-sum(p*log(p)),round(sum(counted$n)))
                      } else {
                        e <- c(-1,0)
                      }
                      e
                    })

# amenities <- as.data.table(amenities)

# lead_amenity <- sapply(pred_amen,FUN = function(x) {
#   if (!is_empty(x)) {amenities[x,.N,by = amenity][order(-N),.SD[1,amenity]]
#     }
#   else if (is_empty(x)) { 0 }
# })
#  
# lead_amenity |> as.data.frame() |> group_by(lead_amenity) |> count(sort = TRUE)
# 
# amenities$lead_color <- sapply(amenities$amenity
#                                ,FUN = function(x) {if (x %in% lead_amen) x else "other"}
#                                )
# 
# amenities_list <- lapply(pred_amen,FUN = function(x) {
#   if (!is_empty(x)) {
#     amenities[x,.N,by = amenity]
#   }
#   else if (is_empty(x)) { integer(0) }
# })
# 
# amenities_list_1 <- lapply(1:length(node_ids_per_hex$selected), function(x) integer(0))
# 
# amenities_list_1[which(node_ids_per_hex$selected != "NA")] <- amenities_list
# 
# amenities_list_1 |> rlist::list.save("app/data/amenities_list.rds") 

## 
grid$entropy <- c(-1)
grid$count <- c(0)
# grid$lead_amen <- c("0")

grid$area_hec[isochrone_poly$id] <- isochrone_poly$area_hec

# grid$area_hec |> summary()

# node_ids_per_hex$intersects |> summary()

grid$entropy[isochrone_poly$id] <-
  sapply(entropy,FUN=function(x) x[1]) |> unlist()

grid$count[isochrone_poly$id] <- 
  sapply(entropy, function(x) x[2]) |> unlist()

# grid$lead_amen[node_ids_per_hex[selected != "NA",id]] <- lead_amenity
# grid <- as.data.table(grid)

#### preview

grid <- grid |> sf::st_as_sf(sf_column_name = 'centroid')

grid_clean <- grid |> 
  drop_na(area_hec) 
# |> 
#   dplyr::filter(entropy != -1)

heaviside <- \(x) sapply(x,FUN= \(x) if (x>0) x else 0)

# runif(100,min = -1) |> sapply(heaviside)

grid_clean <- grid_clean |> 
  dplyr::mutate(entropy_dens=heaviside(entropy/area_hec)
                ,dens=heaviside(count/area_hec))


leaflet::leaflet() |> 
  leaflet::addTiles() |> 
  leafgl::addGlPoints(grid$centroid
                        ,color = 'black'
                        ,fillColor = 'dimgray'
                        ,fillOpacity = 1
  )

tmap::tmap_mode('view')
tmap::qtm(grid_clean
          ,fill = 'entropy_dens'
          ,fill.palette='viridis')

tmap::qtm(grid_clean
          ,fill = 'dens'
          ,fill.palette='viridis'
          ,fill.alpha=.5)

####
plot(grid_clean$dens
     ,grid_clean$entropy_dens
     # ,log = 'x'
     ,ylim = c(0,.2)
     )


####
plot(grid_clean$area_hec
     ,grid_clean$entropy
     ,log = 'x'
)

####
plot(grid_clean$area_hec
     ,grid_clean$count
     ,log = 'y'
)

# grid$entropy

fig <- plot_ly(sf::st_drop_geometry(grid_clean), x = ~dens, y = ~entropy_dens, color = ~area_hec, type = 'scatter', mode = 'markers') %>%
  layout(title = "",
         plot_bgcolor='#e5ecf6', 
         xaxis = list( 
           zerolinecolor = '#ffff', 
           zerolinewidth = 2, 
           gridcolor = 'ffff'
           # ,type = "log"
           # ,range = c(0,7)
           ), 
         yaxis = list( 
           zerolinecolor = '#ffff', 
           zerolinewidth = 2, 
           gridcolor = 'ffff'
           # ,range = c(0,.2)
           ))
fig


# grid[,entropy := round(entropy,2)]

# grid |> rlist::list.save("app/data/grid.rds")

# grid |> sf::st_write("app/grid.geojson")

# grid_gl <- grid |> st_as_sf() |> st_cast("POLYGON") |> filter(!duplicated(id))

# grid_gl |> list.save("app/data/grid_gl.rds")

# grid$lead_amen

# grid_test <- dbGetQuery(conn = con
#                         ,statement = "select 'id',entropy,'count', lead_amen, lead_amen_color, ST_AsText(geometry, 4326) from grid;")
# 
# grid_test <- grid_test |> st_as_sf(wkt = length(grid_test))

####

tmap_mode("view")

isochrone_poly[sapply(amenities_list,FUN = is_empty)] |> qtm(fill.alpha = .5)

####

grid_test <- list.load("app/data/grid_gl.rds")

amenities_list_1 <- rlist::list.load("app/data/amenities_list.rds")

id <- 975

couleurs <- factpal(amenities_list[[id]]$lead_color) |> 
  col2rgb() |>
  apply(MARGIN = 2
        ,FUN = function(x) paste0("rgb("
                                  ,paste(x,collapse = ",")
                                  ,")"))

  plot_ly(data = amenities_list[[id]]
          ,type = 'pie'
          ,labels = ~lead_color
          # ,name = ~amenity
          ,values = ~N
          ,width = "auto"
          ,height = "35%"
          ,insidetextfont = list(color = '#000000')
          # ,hoverinfo = ~paste0("count: ",N)
          ,text = ~amenity
          ,marker = list(colors = couleurs,
                         line = list(color = '#FFFFFF', width = 1))
          ,showlegend = FALSE
          ,alpha = .8
  ) |> layout(title = 'Amenitites distribution',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
### London
  
bbox_to_polygon <- function(bbox) {
  
}

london_shape <- grid |> st_geometry() |> st_bbox() |> st_as_sfc()
  
london_shape |> qtm(fill.alpha = .7)

london_shape |> st_as_sf() |> st_write("app/data/london_shape.geojson"
                                       ,delete_dsn =TRUE)

london_shape |> st_geometry_type()
  
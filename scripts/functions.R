#### Loading and manipulating OSM data ####

# facilitated osm query 
getOSMdata <- function(bb,k, val = "all") {
  
  if (val != "all") { 
    osmdata::opq(bbox = bb,timeout = 600) |>
      osmdata::add_osm_feature(key = k
                      ,value = val
                      ,key_exact = TRUE
                      ,value_exact = TRUE
                      ,match_case = TRUE
      ) |> osmdata::osmdata_sf() 
  } else { 
    osmdata::opq(bbox = bb,timeout = 600) |>
      osmdata::add_osm_feature(key = k
                      #,value = val
                      ,key_exact = TRUE
                      #,value_exact = TRUE
                      #,match_case = TRUE
      ) |> osmdata::osmdata_sf() 
  } 
}

osm_group_points <- function(d,k,kofinterest) {
  df <- bind_rows(filter(d$osm_points,.data[[k]] %in% kofinterest)
                  ,filter(d$osm_polygons,.data[[k]] %in% kofinterest) |> sf::st_centroid()) 
  
  return(df[c("osm_id","name",k,"geometry")] |> rename("amenity" = k) |>
           sf::st_as_sf())
  
}

#### Entropy calculation ####

# NOT USED 
# neighbors_entropy_par <- function(d, r = 500, cor_num = 1){
#   pts_buf <- sf::st_buffer(d, r)
#   int <- sf::st_intersects(pts_buf, d)
#   registerDoParallel(cor_num)
#   entropy <- foreach(i = 1:nrow(d), .combine=c) %dopar% {
#     counted <- d[int[[i]],] |> dplyr::group_by(amenity) |> dplyr::count()
#     p <- counted$n/sum(counted$n)
#     e <- c(-sum(p*log(p)),sum(counted$n))
#   }
#   stopImplicitCluster()
#   data.frame(matrix(entropy, ncol = 2, byrow = TRUE)) |> rename("entropy" = "X1", "size" = "X2")
# }



neighbors_entropy_iso <- function(d, iso, cor_num = 1){
  # make sure that the isochrones data is perfectly alligned by row with the data.
  int <- sf::st_intersects(iso, d)
  # once we have made the intersection, we need to check that they all intersect at least with their own amenity
  # this is not the case every time because some amenities are in locations with no roads around
  # while the isochrones uses the underlyinig road network to build the areas. 
  checks <- map(int,length) |> unlist() |> tibble()
  # when the intersection is zero, we impose that there is just the amenity for which the isochrone is computed
  bad_values <- which(checks[[1]] == 0)
  # put the index of the amenity itself in the intersectio
  int[bad_values] <- bad_values
  
  # registerDoParallel(cor_num)
  # entropy <- foreach(i = 1:nrow(d), .combine=c) %dopar% {
  #   counted <- d[int[[i]],] |> dplyr::group_by(amenity) |> dplyr::count()
  #   p <- counted$n/sum(counted$n)
  #   e <- c(-sum(p*log(p)),sum(counted$n))
  # }
  d <- data.table::as.data.table(d)
  
  entropy <- mclapply(int, mc.cores = cor_num, FUN = \(i) {
    counted <- d[i,.N,by='amenity']
    p <- counted$N/sum(counted$N)
    e <- c(-sum(p*log(p)),sum(counted$N))
    e
  })
  entropy
  # stopImplicitCluster()
  data.frame(matrix(entropy |> unlist(), ncol = 2, byrow = TRUE)) |> rename("entropy" = "X1", "size" = "X2")
}


#### Neighbourhood formation ####

# using the delaunay network of amenities 
# 
# neighbourhoods_graph <- function(data, ord = 1, cores = 1) {
#   
#   # this function requires igraph,stplanr, and doparallel to run
#   data_t <- st_difference(data)
#   data_t <- data_t[!duplicated(data_t$osm_id),]
#   data_coords <- st_coordinates(data_t) |>data.frame()
#   data_network <- ppp(data_coords[,1]
#                       ,data_coords[,2]
#                       ,as.owin(c(londonbb_27700))) |>delaunayNetwork()
#   g <- igraph::graph_from_adjacency_matrix(data_network$dpath*data_network$m
#                                    ,weighted = TRUE
#                                    ,mode = "undirected")
#   V(g)$entropy <- data_t$entropy
#   V(g)$osm_id<- data_t$osm_id
#   
#   registerDoParallel(cores)
#   max <- foreach(i = 1:length(V(g)), .combine = c) %dopar% {
#     nb <- ego(g, order = ord,nodes = V(g)[i],)
#     m <- which(max(nb[[1]]$entropy) == nb[[1]]$entropy)[1]
#     while(! (1 %in% m)) {
#       nb <- ego(g, order = ord,nodes = nb[[1]][m])
#       m <- which(max(nb[[1]]$entropy) == nb[[1]]$entropy)[1]
#     }
#     nb[[1]][1]$osm_id
#   }
#   stopImplicitCluster()
#   max
# }

# using buffers around amenities. 
# 
# neighbourhoods_buff <- function(data, r = 500, cores = 1) {
#   # pass data here in 27700 CRS
#   registerDoParallel(cores)
#   d_buff <- sf::st_buffer(data,r)
#   int <- sf::st_intersects(d_buff, data)
#   data$ind <- 1:nrow(data)
#   max <- foreach(i = 1:nrow(data), .combine = c) %dopar% {
#     nb <- data[int[[i]],]
#     indice <- i
#     #osmid <- data$osm_id[i]
#     m <- nb$ind[which(max(nb$entropy) == nb$entropy)[1]]
#     while(m != indice) {
#       #nb <- data[int[[which(data$osm_id == m)]],]
#       nb <- data[int[[m]],]
#       indice <- m
#       m <- nb$ind[which(max(nb$entropy) == nb$entropy)[1]]
#     }
#     m
#   }
#   stopImplicitCluster()
#   max
# }


neighbourhoods_iso <- function(data,iso, cores = 1) {
  # pass data here in 27700 CRS
  # make sure that the isochrones data is perfectly alligned by row with the data.
  int <- sf::st_intersects(iso, data)
  # once we have made the intersection, we need to check that they all intersect at least with their own amenity
  # this is not the case every time because some amenities are in locations with no roads around
  # while the isochrones uses the underlyinig road network to build the areas. 
  checks <- map(int,length) |>unlist() |>tibble()
  # when the intersection is zero, we impose that there is just the amenity for which the isochrone is computed
  bad_values <- which(checks[[1]] == 0)
  # put the index of the amenity itself in the intersectio
  int[bad_values] <- bad_values
  data$ind <- 1:nrow(data)
  # registerDoParallel(cores)
  # max <- foreach(i = 1:nrow(data), .combine = c) %dopar% {
  #   nb <- data[int[[i]],]
  #   indice <- i
  #   #osmid <- data$osm_id[i]
  #   m <- nb$ind[which(max(nb$entropy) == nb$entropy)[1]]
  #   while(m != indice) {
  #     #nb <- data[int[[which(data$osm_id == m)]],]
  #     nb <- data[int[[m]],]
  #     indice <- m
  #     m <- nb$ind[which(max(nb$entropy) == nb$entropy)[1]]
  #   }
  #   m
  # }
  # stopImplicitCluster()
  
  max <- mcmapply(1:length(int),int, mc.cores = cores,FUN = \(i,val) {
    nb <- data[val,]
    indice <- i
    #osmid <- data$osm_id[i]
    m <- nb$ind[which(max(nb$entropy) == nb$entropy)[1]]
    while(m != indice) {
      #nb <- data[int[[which(data$osm_id == m)]],]
      nb <- data[int[[m]],]
      indice <- m
      m <- nb$ind[which(max(nb$entropy) == nb$entropy)[1]]
    }
    m
  })
  
  
  max
}



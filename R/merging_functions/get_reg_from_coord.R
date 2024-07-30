
# Get Adm1 and Adm2 regions from coordinates

get_reg_from_coord <- function(data, iso3) {
  
  data$Reg <- NA # Reg variable is not applicable where coords are available
  
  # Set Adm1 regions (if available)
  
  # Get GADM regions
  adm1 <- gadm(iso3, level=1, path=paste0(getwd(), "/data"), version="3.6")
  
  if (!is.null(adm1)) { # If Adm1 regions are available for the country
  
    # Convert Adm1 regions into sf object
    adm1 <- st_as_sf(project(adm1, "EPSG:4326"))[,c("NAME_1", "geometry")]
    
    # Create sf object from lat and lon in data
    point_sf <- st_as_sf(distinct(data[which((!is.na(data$long) & !is.na(data$lat)) & 
                                (data$long!=0 & data$lat!=0)), c("long", "lat", "clust")]), 
                         coords = c("long", "lat"), crs = 4326)
    
    # Calculate the distance between adm1 regions and points
    dist_adm1 <- st_distance(point_sf, adm1)
    
    # Find the closest region
    closest_polygon_ind_adm1 <- apply(dist_adm1, 1, which.min)
    closest_polygon_adm1 <- adm1[closest_polygon_ind_adm1, ]
    
    # Get distances between region and points
    min_dist_adm1 <- dist_adm1[cbind(1:nrow(dist_adm1), closest_polygon_ind_adm1)]
    
    # Combine all results
    result_adm1 <- cbind(point_sf, closest_polygon_adm1, distance=min_dist_adm1)
    result_adm1$distance <- as.numeric(result_adm1$distance)
    
    # Remove distances that are more than 10km (indicates that lat and long are unreliable)
    result_adm1 <- subset(result_adm1, distance<=10000)
    
    # Add regions back into data
    result_merge_adm1 <- data.frame(clust = result_adm1$clust,
                                    Adm1 = result_adm1$NAME_1)
    data <- merge(data, result_merge_adm1, by="clust", all.x=TRUE, sort=FALSE)
    
  } else {
    # If Adm1 regions are not applicable to the country, then set to NA
    data$Adm1 <- NA
  }
  
  # Set Adm2 regions (if available)

  # Get GADM regions
  adm2 <- gadm(iso3, level=2, path=paste0(getwd(), "/data"), version="3.6")
  
  if (!is.null(adm2)) { # If Adm2 regions are available for the country
  
    # Convert Adm2 regions into sf object
    adm2 <- st_as_sf(project(adm2, "EPSG:4326"))[,c("NAME_2", "geometry")]
    
    # Create sf object from lat and lon in data
    point_sf <- st_as_sf(distinct(data[which((!is.na(data$long) & !is.na(data$lat)) & 
                                               (data$long!=0 & data$lat!=0)),
                                       c("long", "lat", "clust")]), 
                         coords = c("long", "lat"), crs = 4326)
    
    # Calculate the distance between regions and points
    dist_adm2 <- st_distance(point_sf, adm2)
    
    # Find the closest region
    closest_polygon_ind_adm2 <- apply(dist_adm2, 1, which.min)
    closest_polygon_adm2 <- adm2[closest_polygon_ind_adm2, ]
    
    # Get distances
    min_dist_adm2 <- dist_adm2[cbind(1:nrow(dist_adm2), closest_polygon_ind_adm2)]
    
    # Combine all results
    result_adm2 <- cbind(point_sf, closest_polygon_adm2, distance=min_dist_adm2)
    result_adm2$distance <- as.numeric(result_adm2$distance)
    
    # Remove distances that are more than 10km
    result_adm2 <- subset(result_adm2, distance<=10000)
    
    # Add regions back into data
    result_merge_adm2 <- data.frame(clust = result_adm2$clust,
                                    Adm2 = result_adm2$NAME_2)
    data <- merge(data, result_merge_adm2, by="clust", all.x=TRUE, sort=FALSE)
  } else {
    # If Adm2 regions are not applicable to the country, then set to NA
    data$Adm2 <- NA
  }
  
  # Return the dataframe with regions added
  data
}

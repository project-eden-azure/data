sumamrise_trips = function( dataset ){
    trip_summary = dataset[ , {
        start_end = c(1,.N)
        
        # Compute the distances
        crow_dist = haversine( rawlat[start_end], rawlng[start_end] )
        path_dist = sum(H_dist)
        path_dist2 = sum(H_dist2)
        
        # Actual arrival times
        timediff = difftime( date_[.N], date_[1], units = "s" )
        timediff = as.numeric(timediff)
    
        # Start/end coordinates
        start_x = rawlat[1]
        start_y = rawlng[1]
        end_x = rawlat[.N]
        end_y = rawlng[.N]
        
        # Time that the trip started
        weekday_ = weekday[1]
        hour_ = hour[1]
        rush_hour_ = rush_hour[1]
        
        # Some speed statistics
        mean_speed = mean( pt_speed[-1], na.rm = T )
        var_speed = var( pt_speed[-1], na.rm = T )
        sampling_rate = mean( time_pt_diff[-1], na.rm = T )
        sampling_rate_var = var( time_pt_diff[-1], na.rm = T )
        
        list( 
            timediff = timediff,
            crow_dist = crow_dist, 
            path_dist = path_dist, path_dist2 = path_dist2,
            weekday = weekday_, hour = hour_, rush_hour = rush_hour_,
            start_x = start_x, start_y = start_y, 
            end_x = end_x, end_y = end_y, 
            sampling_rate = sampling_rate, sampling_rate_var = sampling_rate_var,
            mean_speed = mean_speed, var_speed = var_speed
        )
    }, by = "trj_id" ]
    trip_summary
}

add_landmarks = function( summary_data, big_kmeans ){
    dbscan_big_clusters_mat = as.matrix(big_clusters)
    cluster_names = paste0( "C", big_clusters$cluster )

    summary_data[ , c("trip_start1", "trip_end1") := {
        start_coord = c(start_x, start_y)
        start_coord = matrix( start_coord, ncol = 2, nrow = 1 )
        x = haversine2( start_coord, dbscan_big_clusters_mat[,1:2] )
        closest_pt = which.min(x)
        if ( x[ closest_pt ] < 2 ){
            trip_start = cluster_names[ closest_pt ]
        } else{
            trip_start = "generic"
        }
        
        end_coord = c(end_x, end_y)
        end_coord = matrix( end_coord, ncol = 2, nrow = 1 )
        x = haversine2( end_coord, dbscan_big_clusters_mat[,1:2] )
        closest_pt = which.min(x)
        if ( x[ closest_pt ] < 2 ){
            trip_end = cluster_names[ closest_pt ]
        } else{
            trip_end = "generic"
        }
        list( trip_start, trip_end )
    }, by = "trj_id" ]
    
    summary_data[ , c("trip_start", "trip_end") := {
        trip_start_F = factor( trip_start1 )
        trip_end_F = factor( trip_end1 )
        
        trip_start_F2 = relevel( trip_start_F, "generic" )
        trip_end_F2 = relevel( trip_end_F, "generic" )
        list( trip_start_F2, trip_end_F2 )
    } ]
    
    summary_data[ , c("trip_start1", "trip_end1") := NULL ]
}

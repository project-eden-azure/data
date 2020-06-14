library( data.table )

# load( "../../dataset/all_SNG.RData" )
load( "deriving_landmarks_dbscan.RData" )

add_landmarks = function( big_kmeans, dbscan_clusters, summary_data ){
    dbscan_big_clusters_mat = as.matrix(big_kmeans)
    cluster_names = paste0( "C", big_kmeans$cluster )
    
    summary_data[ , c("trip_start", "trip_end") := {
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
}


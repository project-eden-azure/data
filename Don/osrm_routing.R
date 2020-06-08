library( data.table )
library( osrmr )
source( "utility.R" )

setwd( "../../dataset/" )
load( "all_SNG.RData" )

trip_summary = all_data[ , {
    trip_time = difftime( date_[.N], date_[1], units = "s" )
    trip_time = as.numeric(trip_time)
    list(
        lat1 = rawlat[1], lng1 = rawlng[1],
        lat2 = rawlat[.N], lng2 = rawlng[.N],
        known_dist = sum( H_dist ),
        known_time = trip_time
    )
}, by = "trj_id" ]

trip_summary[ , c("OSMR_dist", "OSMR_duration") := {
    cat( "############\n" )
    status = "Fail"
    attempt = 0
    while( status == "Fail" ){
        attempt = attempt + 1
        message = paste0( "Request: ", trj_id, 
            ". Attempt: ", attempt )
        cat( message, "\n" )

        route_info = osmr_routing( lat1, lng1, lat2, lng2, localhost = FALSE )
        
        if ( ! "try-error" %in% class(route_info) ){
            status = "Success"
        }
        message = paste0( "Status: ", status, "\n" )
        cat( message, "\n" )
    }
    n = which( trj_id == trip_summary$trj_id )
    message = paste0( "Trip ", n, " out of ", length(trip_summary$trj_id) )
    cat( message, "\n" )
    
    route_geom = route_info$routes[[1]]$geometry
    route_path = decode_geom( route_geom, precision = 5 )
    trip_dist = sum( haversine( route_path[,"lat"], route_path[,"lng"] ) )
    trip_duration = route_info$routes[[1]]$duration
    list( OSMR_dist = trip_dist, OSMR_duration = trip_duration )
}, by = "trj_id" ]

save( trip_summary, file = "OSRM.RData")





library( data.table )
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


azure_key = readLines( "../data/keys_and_stuff/azure_maps.txt" )

azure_route = function( lat1, lng1, lat2, lng2, api_key, timeout = 0.5 ){
    route_points = data.table()
    metadata = data.table()
    
    Sys.sleep(timeout)
    stump = "https://atlas.microsoft.com/route/directions/json?subscription-key="
    stump2 = "&api-version=1.0&query="
    coordinates = paste0( lat1, ",", lng1, ":", lat2, ",", lng2 )
    stump3 = "&travelMode=car&computeTravelTimeFor=all"
    
    map_query = paste0( stump, api_key, stump2, coordinates, stump3 )
    
    print( map_query )
    json_return = rjson::fromJSON( file = map_query )
    route_data = json_return$routes[[1]]
    route_points = rbindlist( route_data$legs[[1]]$points )
    metadata = as.data.table( route_data$summary )
    
    list( route = route_points, metadata = metadata)
}

load( "Azure_part.RData" )

trip_summary[ trj_id == 62167 ]

trip_summary[ is.na(azure_dist), c("azure_dist", "azure_ETA") := {
    cat( trj_id, "\n" )
    route = try({
        azure_route( lat1, lng1, lat2, lng2, azure_key )
    })
    if ( "try-error" %in% class( route ) ){
        lat1 = round( lat1, 4 )
        lng1 = round( lng1, 4 )
        lat2 = round( lat2, 4 )
        lng2 = round( lng2, 4 )
        route = try({
            azure_route( lat1, lng1, lat2, lng2, azure_key )
        })
    }
    if ( "try-error" %in% class( route ) ){
        lat1 = round( lat1, 3 )
        lng1 = round( lng1, 3 )
        lat2 = round( lat2, 2 )
        lng2 = round( lng2, 2 )
        route = try({
            azure_route( lat1, lng1, lat2, lng2, azure_key )
        })
    }
    azure_dist = route$metadata$lengthInMeters/1000
    azure_ETA = route$metadata$travelTimeInSeconds
    
    list( azure_dist = azure_dist, azure_ETA = azure_ETA )
}, by = "trj_id" ]

save( trip_summary, file = "Azure_part.RData" )

# Add working hours







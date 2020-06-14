osmr_routing = function( lat1, lng1, lat2, lng2,
    localhost = FALSE, timeout = 0.1, trace = T ){
    
    if (use_localhost == T) {
        address <- "http://localhost:5000"
    }
    else {
        address <- "http://router.project-osrm.org"
    }
    Sys.sleep(timeout)
    
    request_str = paste(address, "/route/v1/driving/", 
        lng1, ",", lat1, ";", lng2, ",", 
        lat2, "?overview=full", sep = "", NULL)
    
    R.utils::withTimeout( {
        request <- try({rjson::fromJSON(file = request_str)}, 
            silent = !trace)
    }, timeout = 5, elapsed = 5, onTimeout = "warning" )
    
    if ( "try-error" %in% class(request) & trace ){
        cat( "Try attempt failed.\n" )
        closeAllConnections()
    }
    request
}

azure_route = function( lat1, lng1, lat2, lng2, api_key, timeout = 0.5 ){
    route_points = data.table()
    metadata = data.table()
    
    Sys.sleep(timeout)
    stump = "https://atlas.microsoft.com/route/directions/json?subscription-key="
    stump2 = "&api-version=1.0&query="
    coordinates = paste0( lat1, ",", lng1, ":", lat2, ",", lng2 )
    stump3 = "&travelMode=car"
    
    map_query = paste0( stump, api_key, stump2, coordinates, stump3 )
    
    print( map_query )
    json_return = rjson::fromJSON( file = map_query )
    route_data = json_return$routes[[1]]
    route_points = rbindlist( route_data$legs[[1]]$points )
    metadata = as.data.table( route_data$summary )
    
    list( route = route_points, metadata = metadata)
}
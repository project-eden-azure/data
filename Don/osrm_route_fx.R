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

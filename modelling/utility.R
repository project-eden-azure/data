haversine = function( lat, lng, radius = 6378.137 ){
    lat_rad = lat * pi / 180
    lng_rad = lng * pi / 180
    
    lat1 = lat_rad[-1]
    lat2 = lat_rad[-length(lat)]
    lng1 = lng_rad[-1]
    lng2 = lng_rad[-length(lng)]
    
    lat_diff = ( lat1 - lat2 )
    lon_diff = (lng1 - lng2)
    
    a = sin( lat_diff/2 )^2 +
        cos(lat1) * cos(lat2) * sin(lon_diff/2) * sin(lon_diff/2)
    c_ = 2 * atan2(sqrt(a), sqrt(1-a))
    d = radius * c_
    d
}

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
        
        # Number of GPS samples
        N = .N
        
        # Some speed statistics
        mean_speed = mean( speed, na.rm = T )
        var_speed = var( speed, na.rm = T )
        sampling_rate = mean( time_diff[-1], na.rm = T )
        sampling_rate_var = var( time_diff[-1], na.rm = T )
        
        list( 
            timediff = timediff,
            crow_dist = crow_dist, path_dist = path_dist, path_dist2 = path_dist2,
            weekday = weekday_, hour = hour_, rush_hour = rush_hour_,
            start_x = start_x, start_y = start_y, 
            end_x = end_x, end_y = end_y, 
            N = N, sampling_rate = sampling_rate, sampling_rate_var = sampling_rate_var,
            mean_speed = mean_speed, var_speed = var_speed
        )
    }, by = "trj_id" ]
    trip_summary
}

join_external_distances = function( summary_data, Azure_data, OSRM_data ){
    summary_data[ Azure_data, c("azure_dist", "azure_eta") :={
        list( i.azure_dist, i.azure_ETA )
    }, on = "trj_id" ]
    summary_data[ OSRM_data, c("OSRM_dist", "OSRM_eta") :={
        list( i.OSRM_dist, i.OSRM_duration )
    }, on = "trj_id" ]
    NULL
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

split_dataset = function( data, seed, p ){
    set.seed(seed)
    n = nrow( data )
    training_set_id = sample( 1:n, n * p )
    training_set = data[ training_set_id ]
    test_set = data[ -training_set_id ]
    list( train = training_set, test = test_set )
}

loglik = function( theta, data, distr ){
    if ( any( theta < 0 ) ) return( 1e5 )
    if ( distr == "dlnorm" ){
        n_loglik = -sum( dlnorm( data, theta[1], theta[2], log = T ) )
    }
    if ( distr == "dgamma" ){
        n_loglik = -sum( dgamma( data, theta[1], theta[2], log = T ) )
    }
    if ( distr == "dinvgamma" ){
        n_loglik = -sum( dinvgamma( data, theta[1], theta[2], log = T ) )
    }
    if ( distr == "dtnorm" ){
        n_loglik = -sum( dtnorm( data, theta[1], theta[2], a = 0, log = T ) )
    }
    n_loglik
}

theoretical_percentiles = function( training_data, test_data, varname ){
    X = training_data[[varname]]
    distr_list = c("dlnorm", "dgamma", "dinvgamma", "dtnorm")
    max_likelihoods = sapply( distr_list, function( distr_ ){
        init = c(1,1)
        if ( distr_ == "dtnorm" ){
            init = c(mean(X), sd(X))
        }
        if ( distr_ == "dlnorm" ){
            init = c(mean(log(X)), sd(log(X)))
        }
        if ( distr_ == "dgamma" ){
            mean_X = mean(X)
            var_X = var(X)
            init = c( mean_X^2 / var_X, mean_X / var_X )
        }
        if ( distr_ == "dinvgamma" ){
            mean_X = mean(1/X)
            var_X = var(1/X)
            init = c( mean_X^2 / var_X, mean_X / var_X )
        }
        distr_params = optim( init, loglik, data = X, distr = distr_ )
        return_ = c( distr_params$par, -distr_params$value )
        names(return_) = c("theta1", "theta2", "loglik")
        return_
    } )
    
    best_distr = which.max(max_likelihoods["loglik",])
    distr_call = gsub( "d", "p", names(best_distr))
    fx = get(distr_call)
    distr_params = max_likelihoods[,best_distr]
    percentiles = fx( X, distr_params[1], distr_params[2] )
    new_varname = paste0( varname, "_p")
    
    training_data[ , eval(new_varname) := percentiles ]
    
    test_X = test_data[[varname]]
    test_percentiles = fx( test_X, distr_params[1], distr_params[2] )
    test_data[ , eval( new_varname) := test_percentiles ]
    
    NULL
}

do_rmse = function( lm_model, test_set, varname ){
    yhat = predict( lm_model, test_set )
    sqrt( mean( ( yhat - test_set[[varname]] )^2 ) )
}


haversine2 = function( loc1, loc2, radius = 6378.137 ){
    loc1_rad = loc1 * pi / 180
    loc2_rad = loc2 * pi / 180
    lat1 = loc1_rad[,1]
    lng1 = loc1_rad[,2]
    lat2 = loc2_rad[,1]
    lng2 = loc2_rad[,2]
    
    lat_diff = ( lat1 - lat2 )
    lon_diff = (lng1 - lng2)
    
    a = sin( lat_diff/2 )^2 +
        cos(lat1) * cos(lat2) * sin(lon_diff/2) * sin(lon_diff/2)
    c_ = 2 * atan2(sqrt(a), sqrt(1-a))
    d = radius * c_
    d
}
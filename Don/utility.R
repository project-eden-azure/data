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

update_covariates_generic = function( dataset ){
    dataset[ , H_dist := {
        h_dist = haversine( rawlat, rawlng )
        c( 0, h_dist  )
    }, by = "trj_id" ]
    dataset[ , H_dist2 := {
        h_dist = haversine( rawlng, rawlat )
        c( 0, h_dist  )
    }, by = "trj_id" ]
    
    dataset[ , date_ := .POSIXct(pingtimestamp) ]
    dataset[ , pingtimestamp := NULL ]
    
    dataset[ , weekday := {
        weekday = format( date_, "%a" )
        weekday = factor( weekday,
            levels = c("Sun", "Mon", "Tue", "Wed",
                "Thu", "Fri", "Sat") )
        weekday
    } ]
    
    dataset[ , hour := {
        hour = format( date_, "%H" )
        hour = as.numeric( hour )
        hour
    } ]
    
    dataset[ , speed_test := {
        speed_est = H_dist / c( 0, as.numeric(diff(date)) )
    }, by = "trj_id" ]
    
    dataset[ , is_weekend := {
        weekday %in% c("Sat", "Sun")
    } ]
    
    dataset[ , rush_hour := {
        rush_hour = rep( "No", .N )
        rush_hour[hour > 7 & hour < 10] = "Morning"
        rush_hour[hour > 17 & hour < 20] = "Night"
        factor( rush_hour )
        } ]
    
    dataset
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
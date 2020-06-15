make_historical_data = function(dataset){
    historicals = dataset[ , {
        list( 
            historic_timediff = mean( timediff ),
            historic_crow_dist = mean( crow_dist ),
            historic_path_dist = mean( path_dist ),
            historic_sampling_rate = mean( sampling_rate ),
            historic_mean_speed = mean(mean_speed),
            historic_log_var_speed = mean( log_var_speed )
        )
    }, by = c("weekday", "hour") ]
    historical_vars = colnames( historicals )[
        grepl( "historic", colnames( historicals ) )
    ]
    
    list( historicals = historicals,
        historical_varnames = historical_vars
    )
}

join_historical_data = function( dataset, historical_list ){
    historicals = historical_list$historicals
    historical_vars = historical_list$historical_varnames
    dataset[ historicals, eval(historical_vars) := {
        list( i.historic_timediff,
            i.historic_crow_dist,
            i.historic_path_dist,
            i.historic_sampling_rate,
            i.historic_mean_speed,
            i.historic_log_var_speed
        )
    }, on = c("weekday", "hour") ]
    
}

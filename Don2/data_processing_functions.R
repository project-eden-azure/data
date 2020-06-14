parquet_to_RData = function( parquet_files, RData_file_stump ){
    for( i in 1:length(parquet_files) ){
        cat( "Reading file:", i, "\n" )
        file_ = parquet_files[i]
        cat( file_, "\n" )
        dataset = data.table( read_parquet( file_ ) )
        filename = paste0( RData_file_stump, i, ".RData" )
        save( dataset, file = filename )
    }
}

combine_RData_part_files = function( rdata_files ){
    all_data = NULL
    for ( i in rdata_files ){
        cat( i, "\n" )
        load( i )
        all_data = rbind( all_data, dataset )
    }
    all_data
}

dataset_remove_vars = function( dataset, varlist ){
    cat( "Removing: ", varlist, "\n" )
    dataset[ , eval( varlist ) := NULL ]
    NULL
}

dataset_convert_numeric = function( dataset, varlist ){
    for ( var_ in varlist ){
        cat( "Coercing", var_, "\n" )
        dataset[ , eval(var_) := as.numeric(get(var_)) ]
    }
    NULL
}

dataset_time_vars = function( dataset ){
    cat( "Adding .POSIXct\n" )
    dataset[ , date_ := .POSIXct(pingtimestamp) ]
    
    cat( "Adding weekday\n" )
    dataset[ , weekday := {
        weekday = format( date_, "%a" )
        weekday = factor( weekday,
            levels = c("Sun", "Mon", "Tue", "Wed",
                "Thu", "Fri", "Sat") )
        weekday
    }, by = "trj_id" ]
    
    cat( "Adding hour\n" )
    dataset[ , hour := {
        hour = format( date_, "%H" )
        hour = as.numeric( hour )
        hour
    }, by = "trj_id" ]
    
    cat( "Adding weekend\n" )
    dataset[ , is_weekend := {
        weekday %in% c("Sat", "Sun")
    } ]
    
    NULL
}

dataset_add_rush_hour = function( dataset ){
    cat( "Adding rush_hour\n" )
    dataset[ , rush_hour := {
        rush_hour = rep( "No", .N )
        rush_hour[hour > 7 & hour < 10] = "Morning"
        rush_hour[hour > 17 & hour < 20] = "Night"
        rush_hour[hour > 10 & hour < 17] = "Work"
        factor( rush_hour )
    } ]
    NULL
}

dataset_add_distances = function( dataset ){
    cat( "Adding haversines\n" )
    dataset[ , c("H_dist", "H_dist2") := {
        h_dist = haversine( rawlat, rawlng )
        h1 = c( 0, h_dist  )
        h_dist2 = haversine( rawlng, rawlat )
        h2 = c( 0, h_dist2 )
        list( h1, h2 )
    }, by = "trj_id" ]
    
    NULL
}

dataset_add_timediffs = function( dataset ){
    cat( "Adding time diffs\n" )
    dataset[ , time_pt_diff := {
        time_diff = difftime( date_[-1], date_[-.N], units = "s" )
        time_diff = c( 0, time_diff )
        time_diff
        }, by = "trj_id" ]
    NULL
}

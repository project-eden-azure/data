library( reticulate )
library( data.table )

source_python( "read_parquet_function.py" )

setwd( "../../dataset/")

parquet_files = list.files( pattern = ".parquet", full.names = T )

for( i in 1:length(parquet_files) ){
    cat( "Reading file:", i, "\n" )
    file_ = parquet_files[i]
    cat( file_, "\n" )
    dataset = data.table( read_parquet( file_ ) )
    filename = paste0( "SNG_part_", i, ".RData" )
    save( dataset, file = filename )
}

rdata_files = list.files( pattern = "SNG_part" )
all_data = NULL

for ( i in rdata_files ){
    cat( i, "\n" )
    load( i )
    all_data = rbind( all_data, dataset )
}

all_data[ , bearing := NULL ]
all_data[ , accuracy := NULL ]

rm( list = setdiff( ls(), "all_data") )
gc()

all_data[ , date_ := .POSIXct(pingtimestamp) ]

all_data[ , pingtimestamp := NULL ]
all_data[ , driving_mode := NULL ]

all_data = all_data[ order( trj_id, date_ ) ]

all_data[ , trj_id := as.numeric(trj_id) ]

all_data[ , weekday := {
    weekday = format( date_, "%a" )
    weekday = factor( weekday,
        levels = c("Sun", "Mon", "Tue", "Wed",
            "Thu", "Fri", "Sat") )
    weekday
}, by = "trj_id" ]

all_data[ , hour := {
    hour = format( date_, "%H" )
    hour = as.numeric( hour )
    hour
}, by = "trj_id" ]

all_data[ , is_weekend := {
    weekday %in% c("Sat", "Sun")
} ]

all_data[ , rush_hour := {
    rush_hour = rep( "No", .N )
    rush_hour[hour > 7 & hour < 10] = "Morning"
    rush_hour[hour > 17 & hour < 20] = "Night"
    factor( rush_hour )
} ]


source( "../data/Don/utility.R" )

all_data[ , c("H_dist", "H_dist2") := {
    h_dist = haversine( rawlat, rawlng )
    h1 = c( 0, h_dist  )
    h_dist2 = haversine( rawlng, rawlat )
    h2 = c( 0, h_dist2 )
    list( h1, h2 )
}, by = "trj_id" ]

# all_data[ , H_dist2 := {
#     h_dist = haversine( rawlng, rawlat )
#     c( 0, h_dist  )
# }, by = "trj_id" ]

all_data[ , time_diff := {
    time_diff = difftime( date_[-1], date_[-.N], units = "s" )
    time_diff = c( 0, time_diff )
    time_diff
    }, by = "trj_id" ]

two_window_threshold = function( x, threshold ){
    (x > threshold) & (c( x[-1], F ) > threshold)
}
# all_data[ , c("H_dist", "H_dist2", "speed") := {
#     big_jumps = two_window_threshold(H_dist, 5)
#     H_dist[ big_jumps ] = NaN
#     H_dist2[ big_jumps ] = NaN
#     speed[ big_jumps ] = NaN
#     list( H_dist, H_dist2, speed )
# }, by = "trj_id" ]
# 
# all_data[ osname == "android" & speed == 0, speed := NaN ]

save( all_data, file = "all_SNG.RData" )

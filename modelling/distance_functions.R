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
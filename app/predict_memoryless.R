predict_time <- function(lat1, lng1, lat2, lng2, hour, f_weekday, rush_hour, azure_distance, osrm_distance, crow_distance) {
  OOB_data = data.table(start_x=lat1, start_y=lng1, end_x=lat2, end_y=lng2, hour=hour, weekday=f_weekday, rush_hour=rush_hour, azure_dist=azure_distance, OSRM_dist=osrm_distance, crow_dist=crow_distance)
  
  OOB_data$weekday = factor( OOB_data$weekday,levels = c("Sun", "Mon", "Tue", "Wed","Thu", "Fri", "Sat") )
  OOB_data[ , rush_hour := { rush_hour = rep( "No", .N ); rush_hour[hour > 7 & hour < 10] = "Morning"; rush_hour[hour > 17 & hour < 20] = "Night"; rush_hour[hour > 10 & hour < 17] = "Work"; factor( rush_hour )} ]

  OOB_imputed_path_dist2 = dist_impute_model_predict( impute_path_dist2, OOB_data )
  OOB_data[ , path_dist2_impute := OOB_imputed_path_dist2 ]
  
  # Impute distances
  OOB_impute_path_dist = dist_impute_model_predict( impute_path_dist, OOB_data )
  OOB_data[ , path_dist_impute := OOB_impute_path_dist ]
  
  join_historical_data( OOB_data, historical_data )
  
  # Impute speed
  OOB_imputed_speed = speed_impute_model_predict( impute_speed, OOB_data )
  OOB_data[ , mean_speed_impute := OOB_imputed_speed ]
  
  # Get prediction
  final_predictions = stack_predictions( stacking_model, OOB_data )
  return(final_predictions)
}

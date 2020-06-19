predict_time <- function(lat1, lng1, lat2, lng2, hour, f_weekday, rush_hour, azure_distance, osrm_distance, crow_distance) {
  OOB_data = data.frame(lat1, lng1, lat2, lng2, hour, f_weekday, rush_hour, azure_distance, osrm_distance, crow_distance)
  load( "model/impute_dist2.RData" )
  source( "model/impute_distances.R" )
  OOB_imputed_path_dist2 = dist_impute_model_predict( impute_path_dist2, OOB_data )
  OOB_data[ , path_dist2_impute := OOB_imputed_path_dist2 ]
  rm( impute_path_dist2, OOB_imputed_path_dist2 )
  
  # Impute distances
  load( "model/impute_dist.RData" )
  OOB_impute_path_dist = dist_impute_model_predict( impute_path_dist, OOB_data )
  OOB_data[ , path_dist_impute := OOB_impute_path_dist ]
  rm( impute_path_dist, OOB_impute_path_dist )
  
  load( "model/historical.RData" )
  source( "model/historical.R" )
  join_historical_data( OOB_data, historical_data )
  
  # Impute speed
  load( "model/impute_speed_model.RData" )
  source( "model/impute_speed.R" )
  OOB_imputed_speed = speed_impute_model_predict( impute_speed, OOB_data )
  OOB_data[ , mean_speed_impute := OOB_imputed_speed ]
  rm( impute_speed, speed_impute_model_predict, historical_data )
  
  # Get prediction
  load( "model/stack_model2.RData" )
  source( "model/stack_training.R" )
  final_predictions = stack_predictions( stacking_model, OOB_data )
  return(final_predictions)
}

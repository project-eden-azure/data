library( data.table )
library( caret )
library( mgcv )
library( pls )
library( ranger )
library( randomGLM )
library( xgboost )
library( brnn )
library( mgcv )

load( "G:/azure_hackathon/datasets2/trip_summary/trip_summary2_landmark.RData" )
source( "G:/azure_hackathon/data/Don2/historical.R" )
historical_data = make_historical_data( trip_summary )

save( historical_data,
    file = "G:/azure_hackathon/datasets2/production/historical.RData" )

trip_other_vars = trip_summary[ , list( trj_id, path_dist, mean_speed, timediff, path_dist2 ) ]
vars_to_rm = c("timediff", "path_dist", "path_dist2",
    "sampling_rate_var", "log_sampling_rate_var", "mean_speed", "log_var_speed",
    "sampling_rate",
    "trip_start", "trip_end")
trip_summary[ , eval(vars_to_rm) := NULL ]

trip_data = trip_summary
trip_data_extra = trip_other_vars
rm( trip_summary )

set.seed( 564 )
cv_folds = 7
cv_fold_id = createFolds( trip_data$trj_id, k = cv_folds, returnTrain = T )
train_control = trainControl( 
    method = "cv", number = cv_folds,
    verboseIter = TRUE, 
    search = "grid", 
    index = cv_fold_id, savePredictions = "final",
    returnData = FALSE
)

save( trip_data, trip_data_extra, train_control,
    file = "G:/azure_hackathon/datasets2/production/training.RData" )
rm(list=ls())
gc()


library(doParallel)
cl <- makePSOCKcluster(7, outfile = "out.txt")
registerDoParallel(cl)

##### Impute dist #####

load( "G:/azure_hackathon/datasets2/model_final/training.RData" )
source( "G:/azure_hackathon/data/Don2/impute_distances.R" )

trip_data[ trip_data_extra, path_dist2 := i.path_dist2, on = "trj_id" ]
trip_data[ , trj_id := NULL ]

impute_path_dist2 = train_dist_impute_model( 2, train_control, trip_data )
imputed_path_dist2 = dist_impute_model_predict( impute_path_dist2, trip_data )

trip_data_extra[ , path_dist2_impute := imputed_path_dist2 ]
trip_data[ , path_dist2_impute := imputed_path_dist2 ]
trip_data[ , path_dist2 := NULL ]

trip_data[ , path_dist := trip_data_extra$path_dist ]
impute_path_dist = train_dist_impute_model( 1, train_control, trip_data )
imputed_path_dist = dist_impute_model_predict( impute_path_dist, trip_data )

trip_data_extra[ , path_dist_impute := imputed_path_dist ]
trip_data[ , path_dist_impute := imputed_path_dist ]
trip_data[ , path_dist := NULL ]

save( impute_path_dist,
  file = "G:/azure_hackathon/datasets2/production/impute_dist.RData" )
save( impute_path_dist2, 
  file = "G:/azure_hackathon/datasets2/production/impute_dist2.RData" )
save( trip_data, trip_data_extra, train_control,
  file = "G:/azure_hackathon/datasets2/production/training2_distimpute.RData" )

rm( list = ls() )
gc()

##### Speed ######

load( "G:/azure_hackathon/datasets2/production/training2_distimpute.RData" )
load( "G:/azure_hackathon/datasets2/production/historical.RData" )
source( "G:/azure_hackathon/data/Don2/historical.R" )
source( "G:/azure_hackathon/data/Don2/impute_speed.R" )

trip_data = data.table(trip_data)
join_historical_data( trip_data, historical_data )
trip_data[ , mean_speed := trip_data_extra$mean_speed ]

impute_speed = train_speed_impute_model( train_control, trip_data )
imputed_speed = speed_impute_model_predict( impute_speed, trip_data )

trip_data_extra[ , mean_speed_impute := imputed_speed ]
trip_data[ , mean_speed_impute := imputed_speed ]
trip_data[ , mean_speed := NULL ]

save( impute_speed, 
  file = "G:/azure_hackathon/datasets2/production/impute_speed_model.RData"
)
save( trip_data, trip_data_extra, train_control,
  file = "G:/azure_hackathon/datasets2/production/training3_speedimpute.RData" )

rm( list = ls() )
gc()

#################

##### Training model ####

load( "G:/azure_hackathon/datasets2/production/training3_speedimpute.RData" )
source( "G:/azure_hackathon/data/Don2/stack_training.R" )

trip_data = data.table( trip_data )
trip_data[ , timediff := trip_data_extra$timediff ]

stacking_model = train_0_models( train_control, trip_data,
  savefile = "G:/azure_hackathon/datasets2/production/submodel/model_part.RData" )
save( stacking_model,
  file = "G:/azure_hackathon/datasets2/production/stack_model.RData")

#################













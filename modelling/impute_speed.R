train_speed_impute_model = function( which_var, train_ctrl, train_set, test = FALSE ){
    cat( "Imputation training for speed", which_var, "\n" )
    # Make the model formulae
    if ( which_var == "mean" ){
        model_formula_lm = mean_pt_speed ~ .*. +
            rush_hour * I(crow_dist^2) +
            I(hour^2) + I(hour^3) +
            I(start_y^2) + I(end_y^2) +
            I( azure_dist^2 ) + rush_hour * I(azure_eta^2) +
            I( OSRM_dist^2 ) + rush_hour * I(OSRM_eta^2) +
            I( path_dist_impute^2 ) + I(path_dist2_impute^2) -
            start_y:end_y - start_x:end_x - start_y:end_x - start_x:end_y - start_x:start_y -
            end_x:end_y
        model_formula_rpart = mean_pt_speed ~ .
        
        training_OOF = data.table( mean_pt_speed = training_set$mean_pt_speed )
        stacking_model_formula = as.formula( "mean_pt_speed ~ ." )
    } else if ( which_var == "var" ) {
        model_formula_lm = log_var_pt_speed ~ .*. +
            rush_hour * I(crow_dist^2) +
            I(hour^2) + I(hour^3) +
            I(start_y^2) + I(end_y^2) +
            I( azure_dist^2 ) + rush_hour * I(azure_eta^2) +
            I( OSRM_dist^2 ) + rush_hour * I(OSRM_eta^2) +
            I( path_dist_impute^2 ) + I(path_dist2_impute^2) -
            start_y:end_y - start_x:end_x - start_y:end_x - start_x:end_y - start_x:start_y -
            end_x:end_y -
            weekday:end_x - weekday:end_y -
            weekday:azure_eta - weekday:OSRM_eta - weekday:hour -
            weekday:crow_dist - weekday:path_dist_impute - weekday:path_dist2_impute -
            crow_dist:start_x - crow_dist:end_x - crow_dist:start_y - crow_dist:end_y -
            rush_hour:path_dist_impute - rush_hour:path_dist2_impute -
            hour:rush_hour -
            end_y:azure_dist - end_y:azure_eta - end_y: - end_y:OSRM_eta -
            start_x:azure_dist - start_x:azure_eta - start_x:OSRM_dist - start_x:OSRM_eta -
            start_x:path_dist_impute - start_x:path_dist2_impute -
            azure_dist:OSRM_dist - azure_dist:OSRM_eta - azure_dist:path_dist_impute -
            azure_dist:path_dist2_impute -
            weekday:start_x - weekday:start_y
        model_formula_rpart = log_var_pt_speed ~ .
        
        training_OOF = data.table( log_var_pt_speed = train_set$log_var_pt_speed )
        stacking_model_formula = as.formula( "log_var_pt_speed ~ ." )
    }
    
    cat( "Imputation training LM\n" )
    lm_ = train( form = model_formula_lm,
        data = train_set,
        metric = "RMSE", method = "lm", trControl = train_ctrl)
    lm_pred = data.table( lm_$pred )
    setorder( lm_pred, rowIndex )
    
    cat( "Imputation training ENET\n" )
    n_enet = ifelse( test, 10, 25 ) 
    if ( which_var == "mean" ){
        enet_tunegrid = data.frame(
            lambda = runif( n_enet, 0, 0.025 ),
            fraction = runif( n_enet, 0.3, 1 )
        )
    } else{
        enet_tunegrid = data.frame(
            lambda = rexp( n_enet, 1/0.0001 ),
            fraction = runif( n_enet, 0.3, 1 )
        )
    }
    enet_ = train( form = model_formula_lm, data = train_set,
        metric = "RMSE", method = "enet", trControl = train_ctrl,
        tuneGrid = enet_tunegrid, standardize = TRUE, intercept = TRUE
        )
    enet_pred = data.table( enet_$pred )
    setorder( enet_pred, rowIndex )
    
    cat( "Imputation training PLS\n" )
    full_X = ncol( model.matrix( model_formula_lm, train_set[1:5,]) )
    pls_tunegrid = data.frame( ncomp = 1:full_X )
    pls_ = train( form = model_formula_lm, data = train_set,
        metric = "RMSE", method = "pls", trControl = train_ctrl,
        tuneGrid = pls_tunegrid
        )
    pls_pred = data.table( pls_$pred )
    setorder( pls_pred, rowIndex )
    
    cat( "Imputation training PCR\n" )
    pcr_tunegrid = data.frame( ncomp = 1:full_X )
    pcr_ = train( form = model_formula_lm, data = train_set,
        metric = "RMSE", method = "pcr", trControl = train_ctrl,
        tuneGrid = pcr_tunegrid
    )
    pcr_pred = data.table( pcr_$pred )
    setorder( pcr_pred, rowIndex )
    
    cat( "Imputation training K-MEANS\n" )
    if ( which_var == "mean" ){
        ifelse( test,
            k_grid <- data.frame( k = 15:20 ),
            k_grid <- data.frame( k = 15:50 )
        )
    } else{
        ifelse( test,
            k_grid <- data.frame( k = 40:50 ),
            k_grid <- data.frame( k = 40:100 )
        )
    }
    knn_ = train( form = model_formula_lm, data = train_set,
        metric = "RMSE", method = "knn", trControl = train_ctrl,
        tuneGrid = k_grid
    )
    knn_pred = data.table( knn_$pred )
    setorder( knn_pred, rowIndex )
    
    cat( "Imputation training RPART\n" )
    if ( which_var == "mean" ){
        cp_grid = data.frame( cp = rexp( 100, 1/0.001 ) )
    } else{
        cp_grid = data.frame( cp = rexp( 100, 1/0.005 ) )
    }
    rpart_ = train( model_formula_rpart, data = train_set,
        metric = "RMSE", method = "rpart", trControl = train_ctrl,
        tuneGrid = cp_grid
    )
    rpart_pred = data.table( rpart_$pred )
    setorder( rpart_pred, rowIndex )
    
    cat( "Imputation training GAM\n" )
    if ( test ){
        gam_grid = data.frame( select = F, method = "REML" )
    } else{
        gam_grid = data.frame( select = F,
            method = c( "GACV.Cp", "REML", "ML" )
        )
    }
    gam_ = train( model_formula_rpart, data = train_set,
        metric = "RMSE", method = "gam", trControl = train_ctrl,
        tuneGrid = gam_grid
    )
    gam_pred = data.table( gam_$pred )
    setorder( gam_pred, rowIndex )
    
    cat( "Training stacking\n" )
    training_OOF$lm = lm_pred$pred
    training_OOF$enet = enet_pred$pred
    training_OOF$pls = pls_pred$pred
    training_OOF$pcr = pcr_pred$pred
    training_OOF$knn = knn_pred$pred
    training_OOF$rpart = rpart_pred$pred
    training_OOF$gam = gam_pred$pred
    
    stacked_tunegrid = data.frame( ncomp = 1:(ncol(training_OOF)-1) )
    stacking = train( stacking_model_formula, data = training_OOF,
        metric = "RMSE", method = "pls", trControl = train_ctrl,
        tuneGrid = stacked_tunegrid
    )
    
    model_list = list(
        lm_ = lm_, enet_ = enet_, pls_ = pls_, 
        pcr_ = pcr_, knn_ = knn_, rpart_ = rpart_,
        gam_ = gam_
    )
    list( models0 = model_list, model1 = stacking )
}

speed_impute_model_predict = function( model_list, test_set_ ){
    model0_pred = lapply( model_list$models0, function( model_ ){
        predict( model_, test_set_ )
    } )
    model0_pred = as.data.table( model0_pred )
    colnames(model0_pred) = gsub( "_", "", colnames(model0_pred) )
    
    imputations = predict( model_list$model1, model0_pred )
    imputations
}







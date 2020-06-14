train_dist_impute_model = function( which_dist, train_ctrl, train_set, test = FALSE ){
    cat( "Imputation training for path_dist", which_dist, "\n" )
    # Make the model formulae
    if ( which_dist == 2 ){
        model_formula_lm = path_dist2 ~ .*. +
            I(start_x^2) + I(start_y^2) + I(end_x^2) +  I(end_y^2) +
            I(hour^2) + I(hour^3)  -
            start_x:end_x - start_x:start_y - start_x:end_y -
            start_y:end_x - start_y:start_y - start_y:end_y
        
        model_formula_enet = path_dist2 ~ . +
            I(start_x^2) + I(start_y^2) + I(end_x^2) +  I(end_y^2) +
            I(hour^2) + I(hour^3)  -
            start_x:end_x - start_x:start_y - start_x:end_y -
            start_y:end_x - start_y:start_y - start_y:end_y
        
        model_formula_rpart = path_dist2 ~ .
        
        training_OOF = data.table( path_dist2 = train_set$path_dist2 )
        stacking_model_formula = as.formula( "path_dist2 ~ ." )
    } else if (which_dist == 1) {
        model_formula_lm = path_dist ~ .*. +
            I(start_x^2) + I(start_y^2) + I(end_x^2) +  I(end_y^2) +
            I(hour^2) + I(hour^3)  -
            start_x:end_x - start_x:start_y - start_x:end_y -
            start_y:end_x - start_y:start_y - start_y:end_y
        
        model_formula_enet = path_dist ~ . +
            I(start_x^2) + I(start_y^2) + I(end_x^2) +  I(end_y^2) +
            I(hour^2) + I(hour^3)  -
            start_x:end_x - start_x:start_y - start_x:end_y -
            start_y:end_x - start_y:start_y - start_y:end_y
        
        model_formula_rpart = path_dist ~ .
        
        training_OOF = data.table( path_dist = train_set$path_dist )
        stacking_model_formula = as.formula( "path_dist ~ ." )
    }
    
    cat( "Imputation training LM\n" )
    lm_ = train( form = model_formula_lm,
        data = train_set,
        metric = "RMSE", method = "lm", trControl = train_ctrl)
    lm_pred = data.table( lm_$pred )
    setorder( lm_pred, rowIndex )
    
    ################
    cat( "Imputation training ENET\n" )
    if ( test ){
        n_enet = 5
    } else{
        n_enet = 50
    }
    enet_tunegrid = data.frame(
        lambda = rexp( n_enet, 1/0.0001 ),
        fraction = runif( n_enet, 0.9, 1 )
    )
    enet_ = train( form = model_formula_enet, data = train_set,
        metric = "RMSE", method = "enet", trControl = train_ctrl,
        tuneGrid = enet_tunegrid, standardize = TRUE, intercept = TRUE
        )
    enet_pred = data.table( enet_$pred )
    setorder( enet_pred, rowIndex )
    
    ###############
    cat( "Imputation training PLS\n" )
    full_X = ncol( model.matrix( model_formula_enet, train_set[1:5,]) )
    pls_tunegrid = data.frame( ncomp = 1:full_X )
    pls_ = train( form = model_formula_enet, data = train_set,
        metric = "RMSE", method = "pls", trControl = train_ctrl,
        tuneGrid = pls_tunegrid
        )
    pls_pred = data.table( pls_$pred )
    setorder( pls_pred, rowIndex )
    
    cat( "Imputation training PCR\n" )
    
    pcr_tunegrid = data.frame( ncomp = 1:full_X )
    pcr_ = train( form = model_formula_enet, data = train_set,
        metric = "RMSE", method = "pcr", trControl = train_ctrl,
        tuneGrid = pcr_tunegrid
    )
    pcr_pred = data.table( pcr_$pred )
    setorder( pcr_pred, rowIndex )
    
    ###############
    cat( "Imputation training K-MEANS\n" )
    if ( test ){
        k_grid = data.frame( k = 5:10 )
    } else{
        k_grid = data.frame( k = 5:30 )
    }
    knn_ = train( form = model_formula_rpart, data = train_set,
        metric = "RMSE", method = "knn", trControl = train_ctrl,
        tuneGrid = k_grid
    )
    knn_pred = data.table( knn_$pred )
    setorder( knn_pred, rowIndex )
    
    ###############
    cat( "Imputation training RPART\n" )
    cp_grid = data.frame( cp = rexp( 100, 1/0.001 ) )
    rpart_ = train( model_formula_rpart, data = train_set,
        metric = "RMSE", method = "rpart", trControl = train_ctrl,
        tuneGrid = cp_grid
    )
    rpart_pred = data.table( rpart_$pred )
    setorder( rpart_pred, rowIndex )
    
    ####################
    cat( "Imputation training GAM\n" )
    gam_grid = data.frame( select = F,
        method = c( "GACV.Cp", "REML", "ML" )
    )
    gam_ = train( model_formula_rpart, data = train_set,
        metric = "RMSE", method = "gam", trControl = train_ctrl,
        tuneGrid = gam_grid
    )
    gam_pred = data.table( gam_$pred )
    setorder( gam_pred, rowIndex )
    
    ###################
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

dist_impute_model_predict = function( model_list, test_set_ ){
    model0_pred = lapply( model_list$models0, function( model_ ){
        predict( model_, test_set_ )
    } )
    model0_pred = as.data.table( model0_pred )
    colnames(model0_pred) = gsub( "_", "", colnames(model0_pred) )
    
    imputations = predict( model_list$model1, model0_pred )
    imputations
}







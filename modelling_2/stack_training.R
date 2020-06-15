training_0_formulas = function(){
    
    model_formula_lm = timediff ~ . +
        hour * I(crow_dist^2) +
        hour * I(path_dist_impute^2) +
        I(hour^2) + I(hour^3) +
        I( azure_dist^2 ) + I( OSRM_dist^2 ) +
        I( path_dist_impute^2 ) + I(path_dist2_impute^2) +
        I(start_x^2) + I( start_y^2 ) + I( end_x^2 ) + I( end_y^2 ) +
        I( mean_speed_impute^2 ) +
        crow_dist:weekday + crow_dist:hour + crow_dist:rush_hour + 
        crow_dist:start_x + crow_dist:start_y + crow_dist:end_x + crow_dist:end_y +
        crow_dist:azure_dist + crow_dist:OSRM_dist + crow_dist:path_dist2_impute +
        crow_dist:path_dist_impute + crow_dist:historic_timediff + crow_dist:historic_crow_dist +
        crow_dist:historic_path_dist + crow_dist:mean_speed_impute +
        weekday:hour +
        weekday:path_dist_impute + weekday:path_dist2_impute + weekday:azure_dist + weekday:OSRM_dist +
        weekday:historic_crow_dist +
        hour:start_x + hour:start_y + hour:end_x + hour:end_y +
        hour:historic_mean_speed + hour:mean_speed_impute +
        rush_hour:path_dist_impute + rush_hour:path_dist2_impute + 
        rush_hour:azure_dist + rush_hour:OSRM_dist +
        mean_speed_impute:start_x + mean_speed_impute:start_y + mean_speed_impute:end_x + mean_speed_impute:end_y +
        mean_speed_impute:path_dist_impute + mean_speed_impute:path_dist2_impute + 
        mean_speed_impute:azure_dist + mean_speed_impute:OSRM_dist

    simple_model_formula_lm = timediff ~ . +
        I(crow_dist^2) +
        I(path_dist_impute^2) +
        I(hour^2) + I(hour^3) +
        I( azure_dist^2 ) + I( OSRM_dist^2 ) +
        I( path_dist_impute^2 ) + I(path_dist2_impute^2) +
        I(start_x^2) + I( start_y^2 ) + I( end_x^2 ) + I( end_y^2 ) +
        I( mean_speed_impute^2 )

    rpart_model_formula = timediff ~ .
    
    list( interaction_model = model_formula_lm,
        polynomial_model = simple_model_formula_lm,
        base_model = rpart_model_formula
    )
}

add_pred_to_OOF = function( data_OOF, model_obj, model_name ){
    model_pred = data.table( model_obj$pred )
    setorder( model_pred, rowIndex )
    data_OOF[ , eval(model_name) := model_pred$pred ] 
}

train_0_models = function( train_ctrl, train_set, test = FALSE, savefile ){
    
    cat( "Imputation training for level0", "\n" )
    model_formulas = training_0_formulas()
    stacking_model_formula = timediff ~ .
    
    training_OOF = data.table( timediff = train_set$timediff )

    model_list = list()

    ###########################################
    # model_n = 1
    # for ( model_form in model_formulas[2] ){
    #     cat( "Training LM ", model_n, "\n" )
    #     model_name = "lm"
    # 
    #     lm_x = train( form = model_form,
    #         data = train_set,
    #         metric = "RMSE", method = "lm", trControl = train_ctrl)
    #     model_name = paste0( model_name, model_n )
    #     add_pred_to_OOF( training_OOF, lm_x, model_name )
    #     model_n = model_n + 1
    #     model_list[[model_name]] = lm_x
    # }
    # 
    # save( model_list, training_OOF, file = savefile )
    # print( names(model_list) )

    ########################################
    # model_n = 1
    # for ( model_form in model_formulas[2] ){
    #     cat( "Training ENET ", model_n, "\n" )
    #     model_name = "enet"
    # 
    #     n_enet = ifelse( test, 5, 50 )
    #     tune_grid = data.frame(
    #         lambda = rexp( n_enet, 1/1e-8 ),
    #         fraction = runif( n_enet, 0.75, 1 )
    #     )
    #     enet_x = train( form = model_form, data = train_set,
    #         metric = "RMSE", method = "enet", trControl = train_ctrl,
    #         tuneGrid = tune_grid, standardize = TRUE, intercept = TRUE
    #     )
    #     model_name = paste0( model_name, model_n )
    #     add_pred_to_OOF( training_OOF, enet_x, model_name )
    #     model_n = model_n + 1
    #     model_list[[model_name]] = enet_x
    # }
    # save( model_list, training_OOF, file = savefile )
    # print( names(model_list) )

    ########################################
    # model_n = 1
    # for ( model_form in model_formulas[3] ){
    #     cat( "Training PLS ", model_n, "\n" )
    #     model_name = "PLS"
    # 
    #     full_X = ncol( model.matrix( model_form, train_set[1,]) )
    #     tune_grid = data.frame( ncomp = 1:full_X )
    #     pls_x = train( form = model_form, data = train_set,
    #         metric = "RMSE", method = "pls", trControl = train_ctrl,
    #         tuneGrid = tune_grid, scale = T
    #     )
    #     model_name = paste0( model_name, model_n )
    #     add_pred_to_OOF( training_OOF, pls_x, model_name )
    #     model_n = model_n + 1
    #     model_list[[model_name]] = pls_x
    # }
    # save( model_list, training_OOF, file = savefile )
    # print( names(model_list) )

    #########################################
    model_n = 1
    for ( model_form in model_formulas[3] ){
        cat( "Training PCR ", model_n, "\n" )
        model_name = "PCR"

        full_X = ncol( model.matrix( model_form, train_set[1,]) )
        tune_grid = data.frame( ncomp = 1:full_X )
        pcr_x = train( form = model_form, data = train_set,
            metric = "RMSE", method = "pcr", trControl = train_ctrl,
            tuneGrid = tune_grid, scale = T
        )
        model_name = paste0( model_name, model_n )
        add_pred_to_OOF( training_OOF, pcr_x, model_name )
        model_n = model_n + 1
        model_list[[model_name]] = pcr_x
    }
    save( model_list, training_OOF, file = savefile )
    print( names(model_list) )

    ##########################################
    model_n = 1
    for ( model_form in model_formulas[3] ){
        cat( "Training KNN ", model_n, "\n" )
        model_name = "KNN"

        kmax = ifelse( test, 15, 30 )
        tune_grid = data.frame( k = 10:kmax )
        knn_x = train( form = model_form, data = train_set,
            metric = "RMSE", method = "knn", trControl = train_ctrl,
            tuneGrid = tune_grid
        )
        model_name = paste0( model_name, model_n )
        add_pred_to_OOF( training_OOF, knn_x, model_name )
        model_n = model_n + 1
        model_list[[model_name]] = knn_x
    }
    save( model_list, training_OOF, file = savefile )
    print( names(model_list) )

    ##########################################
    model_n = 1
    for ( model_form in model_formulas[3] ){
        cat( "Training Rpart ", model_n, "\n" )
        model_name = "Rpart"

        tune_grid = data.frame( cp = rexp( 100, 1/0.005 ) )
        rpart_x = train( model_form,
            data = train_set,
            metric = "RMSE", method = "rpart", trControl = train_ctrl,
            tuneGrid = tune_grid
        )
        model_name = paste0( model_name, model_n )
        add_pred_to_OOF( training_OOF, rpart_x, model_name )
        model_n = model_n + 1
        model_list[[model_name]] = rpart_x
    }
    save( model_list, training_OOF, file = savefile )
    print( names(model_list) )

    ##########################################
    model_n = 1
    for ( model_form in model_formulas[3] ){
        cat( "Training GAM ", model_n, "\n" )
        model_name = "GAM"

        tune_grid = data.table(
            select = F,
            method = c( "GACV.Cp" )
        )
        gam_x = train( model_form, data = train_set,
            metric = "RMSE", method = "gam", trControl = train_ctrl,
            tuneGrid = tune_grid
        )
        model_name = paste0( model_name, model_n )
        add_pred_to_OOF( training_OOF, gam_x, model_name )
        model_n = model_n + 1
        model_list[[model_name]] = gam_x
    }
    save( model_list, training_OOF, file = savefile )
    print( names(model_list) )

    ##########################################
    model_n = 1
    for ( model_form in model_formulas[3] ){
        cat( "Training brNN ", model_n, "\n" )
        model_name = "brNN"

        tune_grid = data.frame( neurons = 1:5 )
        brnn_x = train( model_form, data = train_set,
            metric = "RMSE", method = "brnn", trControl = train_ctrl,
            tuneGrid = tune_grid, Monte_Carlo = FALSE
        )
        model_name = paste0( model_name, model_n )
        add_pred_to_OOF( training_OOF, brnn_x, model_name )
        model_n = model_n + 1
        model_list[[model_name]] = brnn_x
    }
    save( model_list, training_OOF, file = savefile )
    print( names(model_list) )

    #########################################
    model_n = 1
    for ( model_form in model_formulas[3] ){
        cat( "Training RF ", model_n, "\n" )
        model_name = "RF"

        ranger_n = ifelse( test, 3, 50 )
        tune_grid = data.frame(
            mtry = floor( runif( ranger_n, 12, 23) ),
            splitrule = "variance",
            min.node.size = floor( runif(ranger_n, 5, 60) )
        )
        RF_x = train( model_form, data = train_set,
            metric = "RMSE", method = "ranger", trControl = train_ctrl,
            tuneGrid = tune_grid, num.threads = 2
        )
        model_name = paste0( model_name, model_n )
        add_pred_to_OOF( training_OOF, RF_x, model_name )
        model_n = model_n + 1
        model_list[[model_name]] = RF_x
    }
    save( model_list, training_OOF, file = savefile )
    print( names(model_list) )

    #########################################
    model_n = 1
    for ( model_form in model_formulas[3] ){
        cat( "Training treebag ", model_n, "\n" )
        model_name = "treebag"

        treebag_x = train( model_form, data = train_set,
            metric = "RMSE", method = "treebag", trControl = train_ctrl
        )
        model_name = paste0( model_name, model_n )
        add_pred_to_OOF( training_OOF, treebag_x, model_name )
        model_n = model_n + 1
        model_list[[model_name]] = treebag_x
    }
    save( model_list, training_OOF, file = savefile )
    print( names(model_list) )

    ##########################################
    model_n = 1
    for ( model_form in model_formulas[3] ){
        cat( "Training randomGLM ", model_n, "\n" )
        model_name = "randomGLM"

        tune_grid = data.table( maxInteractionOrder = 1 )
        randomGLM_x = train( model_form, data = train_set,
            metric = "RMSE", method = "randomGLM", trControl = train_ctrl,
            tuneGrid = tune_grid, nThreads = 1
        )
        model_name = paste0( model_name, model_n )
        add_pred_to_OOF( training_OOF, randomGLM_x, model_name )
        model_n = model_n + 1
        model_list[[model_name]] = randomGLM_x
    }
    save( model_list, training_OOF, file = savefile )
    print( names(model_list) )

    # #########################################
    # model_n = 1
    # for ( model_form in model_formulas[3] ){
    #     cat( "Training gaussian process ", model_n, "\n" )
    #     model_name = "gauss_process"
    # 
    #     gauss_process_x = train( model_form, data = train_set,
    #         metric = "RMSE", method = "gaussprLinear", trControl = train_ctrl
    #     )
    #     model_name = paste0( model_name, model_n )
    #     add_pred_to_OOF( training_OOF, gauss_process_x, model_name )
    #     model_n = model_n + 1
    #     model_list[[model_name]] = gauss_process_x
    # }
    # save( model_list, training_OOF, file = savefile )
    # print( names(model_list) )

    ##########################################
    model_n = 1
    for ( model_form in model_formulas[3] ){
        cat( "Training XGB ", model_n, "\n" )
        model_name = "XGB"

        xgboost_tree_n = ifelse( test, 5, 50 )
        tune_grid = data.frame(
            nrounds = floor( runif( xgboost_tree_n, 200, 280 ) ),
            max_depth = floor( runif( xgboost_tree_n, 3, 7 ) ),
            eta = runif( xgboost_tree_n, 0.05 ),
            gamma = runif( xgboost_tree_n, 5, 40 ),
            colsample_bytree = 0.75,
            subsample = 0.89,
            min_child_weight = runif( xgboost_tree_n, 20, 40 )
        )
        XGB_x = train( model_form, data = train_set,
            metric = "RMSE", method = "xgbTree", trControl = train_ctrl,
            tuneGrid = tune_grid
        )
        model_name = paste0( model_name, model_n )
        add_pred_to_OOF( training_OOF, XGB_x, model_name )
        model_n = model_n + 1
        model_list[[model_name]] = XGB_x
    }
    save( model_list, training_OOF, file = savefile )
    print( names(model_list) )
    
    #########################################
    
    cat( "Training stacking\n" )
    # stacked_tunegrid = data.frame( ncomp = 1:( ncol( training_OOF ) - 1 ) )
    stacked_tunegrid = data.frame( neurons = 1:10 )
    stacking = train( stacking_model_formula, data = training_OOF,
        metric = "RMSE", method = "brnn", trControl = train_ctrl,
        tuneGrid = stacked_tunegrid
    )
    
    save( model_list, training_OOF, file = savefile )
    list( models0 = model_list,
        model1 = stacking )
}

stack_predictions = function( all_stack, test_set_ ){
    model0_pred = lapply( all_stack$models0, function( model_ ){
        predict( model_, test_set_ )
    } )
    model0_pred = as.data.table( model0_pred )

    stack_pred = predict( all_stack$model1, model0_pred )
    cbind( stack = stack_pred, model0_pred )
}



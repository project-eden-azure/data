library( data.table )

load( "G:/azure_hackathon/datasets2/trip_summary/trip_summary2_landmark.RData" )
load( "G:/azure_hackathon/datasets2/model_final/historical.RData" )
source( "G:/azure_hackathon/data/Don2/historical.R" )

trip_summary = data.table(trip_summary)
join_historical_data( trip_summary, historical_data )


# dataset1 = trip_summary[ 1:1000 ]
dataset1 = trip_summary
dataset1[ , crow_dist.2 := crow_dist^2 ]
dataset1[ , azure_dist.2 := azure_dist^2 ]
dataset1[ , OSRM_dist.2 := OSRM_dist^2 ]
dataset1[ , OSRM_dist.2 := OSRM_dist^2 ]
dataset1[ , OSRM_dist.2 := OSRM_dist^2 ]
dataset1[ , c("sin1", "cos1", "sin2", "cos2", "sin3", "cos3") := {
    list( 
        sin( 2 * pi * 1 * hour / 24 ), cos( 2 * pi * 1 * hour / 24 ),
        sin( 2 * pi * 2 * hour / 24 ), cos( 2 * pi * 2 * hour / 24 ),
        sin( 2 * pi * 3 * hour / 24 ), cos( 2 * pi * 3 * hour / 24 )
    )
} ]

dataset1[ , log_path_dist := log(path_dist) ]
dataset1[ , log_mean_speed := log(mean_speed) ]
dataset1[ , log_timediff := log(timediff) ]


timediffs = dataset1$timediff


factor_vars = dataset1[ , list( weekday, rush_hour, trip_start, trip_end ) ]
numeric_x = dataset1[ , mget( setdiff( names( dataset1 ), names(factor_vars ) ) ) ]
numeric_x[ , c("trj_id", "timediff") := NULL ]
numeric_x[ , c("sampling_rate", "log_sampling_rate_var", "historic_crow_dist",
    "historic_path_dist", "historic_mean_speed", "mean_speed",
    "path_dist", "path_dist2", "log_var_speed", "log_timediff",
    "hour") := NULL ]
numeric_x[ , c("start_x", "end_y") := NULL ]

numeric_x[ , c("crow_dist", "start_y", "end_x",
    "historic_timediff", "crow_dist.2", "historic_sampling_rate") := NULL ]

log_timediffs = log( timediffs )

numeric_x = scale( numeric_x, scale = FALSE )
scale_means = attr( numeric_x, "scaled:center")

jags_data = cbind( as.data.table(numeric_x), factor_vars,
    log_timediff = log_timediffs )

jags_model_str = "model{
    dist_tau ~ dgamma( 0.0001, 0.0001 )
    speed_tau ~ dgamma( 0.0001, 0.0001 )
    eta_tau ~ dgamma( 0.0001, 0.0001 )
    
    for ( i in 1:10 ){
        dist_betas[i] ~ dnorm( 0, dist_tau )
    }
    for ( i in 1:12 ){
        speed_betas[i] ~ dnorm( 0, speed_tau )
    }
    for ( i in 1:13 ){
        eta_betas[i] ~ dnorm( 0, eta_tau )
    }

    for ( i in 1:7 ){
        dist_weekday_betas[i] ~ dnorm( 0, dist_tau )
        
        speed_weekday_betas[i] ~ dnorm( 0, speed_tau )
        speed_sin1_x_weekday[i] ~ dnorm( 0, speed_tau )
        speed_sin3_x_weekday[i] ~ dnorm( 0, speed_tau )
        speed_cos3_x_weekday[i] ~ dnorm( 0, speed_tau )
        
        eta_weekday_betas[i] ~ dnorm( 0, eta_tau )
        eta_sin2_x_weekday[i] ~ dnorm( 0, eta_tau )
    }

    for ( i in 1:4 ){
        # dist_rush_betas[i] ~ dnorm( 0, speed_tau )
        speed_rush_betas[i] ~ dnorm( 0, speed_tau )
        eta_rush_betas[i] ~ dnorm( 0, eta_tau )
    }

    for ( i in 1:8 ){
        dist_sin1_x_trip_start[i] ~ dnorm( 0, dist_tau )
        dist_cos1_x_trip_start[i] ~ dnorm( 0, dist_tau )
        dist_sin2_x_trip_start[i] ~ dnorm( 0, dist_tau )
        dist_cos2_x_trip_start[i] ~ dnorm( 0, dist_tau )
        dist_sin3_x_trip_start[i] ~ dnorm( 0, dist_tau )
        dist_cos3_x_trip_start[i] ~ dnorm( 0, dist_tau )
        dist_sin1_x_trip_end[i] ~ dnorm( 0, dist_tau )
        dist_cos1_x_trip_end[i] ~ dnorm( 0, dist_tau )
        dist_sin2_x_trip_end[i] ~ dnorm( 0, dist_tau )
        dist_cos2_x_trip_end[i] ~ dnorm( 0, dist_tau )
        dist_sin3_x_trip_end[i] ~ dnorm( 0, dist_tau )
        dist_cos3_x_trip_end[i] ~ dnorm( 0, dist_tau )

        speed_tripstart_betas[i] ~ dnorm( 0, speed_tau )
        speed_tripend_betas[i] ~ dnorm( 0, speed_tau )
        
        speed_sin1_x_trip_start[i] ~ dnorm( 0, speed_tau )
        speed_cos1_x_trip_start[i] ~ dnorm( 0, speed_tau )
        speed_sin2_x_trip_start[i] ~ dnorm( 0, speed_tau )
        speed_sin3_x_trip_start[i] ~ dnorm( 0, speed_tau )
        speed_sin1_x_trip_end[i] ~ dnorm( 0, speed_tau )
        speed_cos1_x_trip_end[i] ~ dnorm( 0, speed_tau )
        speed_cos2_x_trip_end[i] ~ dnorm( 0, speed_tau )
        speed_sin3_x_trip_end[i] ~ dnorm( 0, speed_tau )
        speed_cos3_x_trip_end[i] ~ dnorm( 0, speed_tau )

        eta_tripstart_betas[i] ~ dnorm( 0, eta_tau )
        eta_tripend_betas[i] ~ dnorm( 0, eta_tau )
    }

    for ( obs in 1:length(log_timediff) ){
        path_dist_mu[obs] = 
            azure_dist[obs] * dist_betas[1] +
            azure_dist.2[obs] * dist_betas[2] +
            OSRM_dist[obs] * dist_betas[3] +
            OSRM_dist.2[obs] * dist_betas[4] +
            sin1[obs] * dist_betas[5] + 
            cos1[obs] * dist_betas[6] +
            sin2[obs] * dist_betas[7] +
            cos2[obs] * dist_betas[8] +
            sin3[obs] * dist_betas[9] +
            cos3[obs] * dist_betas[10] +
            dist_weekday_betas[weekday[obs]] +
            dist_sin1_x_trip_start[trip_start[obs]] * sin1[obs] +
            dist_cos1_x_trip_start[trip_start[obs]] * cos1[obs] +
            dist_sin2_x_trip_start[trip_start[obs]] * sin2[obs] +
            dist_cos2_x_trip_start[trip_start[obs]] * cos2[obs] +
            dist_sin3_x_trip_start[trip_start[obs]] * sin3[obs] +
            dist_cos3_x_trip_start[trip_start[obs]] * cos3[obs] +
            dist_sin1_x_trip_end[trip_end[obs]] * sin1[obs] +
            dist_cos1_x_trip_end[trip_end[obs]] * cos1[obs] +
            dist_sin2_x_trip_end[trip_end[obs]] * sin2[obs] +
            dist_cos2_x_trip_end[trip_end[obs]] * cos2[obs] +
            dist_sin3_x_trip_end[trip_end[obs]] * sin3[obs] +
            dist_cos3_x_trip_end[trip_end[obs]] * cos3[obs]

        log_path_dist[obs] ~ dnorm( path_dist_mu[obs], dist_tau )
        log_path_dist_pred[obs] ~ dnorm( path_dist_mu[obs], dist_tau )
        
        mean_speed_mu[obs] = 
            speed_betas[1] +
            OSRM_dist[obs] * speed_betas[2] +
            OSRM_dist.2[obs] * speed_betas[3] +
            historic_log_var_speed[obs] * speed_betas[4] +
            log_path_dist[obs] * speed_betas[5] +
            sin1[obs] * speed_betas[6] +
            cos1[obs] * speed_betas[7] +
            sin2[obs] * speed_betas[8] +
            cos2[obs] * speed_betas[9] +
            sin3[obs] * speed_betas[10] +
            cos3[obs] * speed_betas[11] +
            exp(log_path_dist[obs]) * speed_betas[12] +
            speed_weekday_betas[weekday[obs]] +
            speed_rush_betas[rush_hour[obs]] +
            speed_tripstart_betas[trip_start[obs]] +
            speed_tripend_betas[trip_end[obs]] +
            speed_sin2_x_trip_start[trip_start[obs]] * sin2[obs] +
            speed_sin3_x_trip_start[trip_start[obs]] * sin3[obs] +
            speed_cos1_x_trip_end[trip_end[obs]] * cos1[obs] +
            speed_sin3_x_trip_end[trip_end[obs]] * sin3[obs] +
            speed_cos3_x_trip_end[trip_end[obs]] * cos3[obs] +
            speed_sin1_x_weekday[weekday[obs]] * sin1[obs] +
            speed_cos3_x_weekday[weekday[obs]] * cos3[obs] +
            speed_sin1_x_trip_start[trip_start[obs]] * sin1[obs] +
            speed_cos1_x_trip_start[trip_start[obs]] * cos1[obs] +
            speed_sin1_x_trip_end[trip_end[obs]] * sin1[obs] +
            speed_cos2_x_trip_end[trip_end[obs]] * cos2[obs] +
            speed_sin3_x_weekday[weekday[obs]] * sin3[obs]

        log_mean_speed[obs] ~ dnorm( mean_speed_mu[obs], speed_tau )
        log_mean_speed_pred[obs] ~ dnorm( mean_speed_mu[obs], speed_tau )
        
        
        #### Removed timediff variables
        timediff_mu[obs] = 
            eta_betas[1] +
            OSRM_dist[obs] * eta_betas[2] +
            OSRM_dist.2[obs] * eta_betas[3] +
            eta_rush_betas[rush_hour[obs]] +
            eta_weekday_betas[weekday[obs]] +
            eta_tripstart_betas[trip_start[obs]] +
            eta_tripend_betas[trip_end[obs]] +
            log_path_dist[obs] * eta_betas[4] +
            log_mean_speed[obs] * eta_betas[5] +
            log_path_dist[obs] / log_mean_speed[obs] * eta_betas[6] +
            (log_path_dist[obs] - log_mean_speed[obs]) * eta_betas[7] +
            sin1[obs] * eta_betas[8] + 
            cos1[obs] * eta_betas[9] +
            sin2[obs] * eta_betas[10] +
            cos2[obs] * eta_betas[11] +
            sin3[obs] * eta_betas[12] +
            cos3[obs] * eta_betas[13] +
            eta_sin2_x_weekday[weekday[obs]] * sin2[obs]
            
        log_timediff[obs] ~ dnorm( timediff_mu[obs], eta_tau )
        log_timediff_pred[obs] ~ dnorm( timediff_mu[obs], eta_tau )
    }
}"

jags_model = jags.model( textConnection(jags_model_str), 
    data =  jags_data,  n.adapt = 50000, n.chains = 1 )

varnames = c(
    "log_timediff_pred", "log_mean_speed_pred", "log_path_dist_pred", "mean_speed_mu[1]", "path_dist_mu[1]",
    "dist_betas", "speed_betas", "eta_betas",
    "dist_weekday_betas", "speed_weekday_betas",
    "eta_weekday_betas",
    "dist_rush_betas", 
    "speed_rush_betas", "eta_rush_betas",
    "dist_tripstart_betas", "dist_tripend_betas",
    "speed_tripstart_betas", "speed_tripend_betas", 
    "eta_tripstart_betas", "eta_tripend_betas",
    "dist_sin1_x_trip_start", "dist_cos1_x_trip_start", "dist_sin2_x_trip_start",
    "dist_cos2_x_trip_start", "dist_sin3_x_trip_start", "dist_cos3_x_trip_start",
    "speed_sin1_x_trip_start", "speed_cos1_x_trip_start", "speed_sin2_x_trip_start",
    "speed_cos2_x_trip_start", "speed_sin3_x_trip_start", "speed_cos3_x_trip_start",
    "eta_sin1_x_trip_start", "eta_cos1_x_trip_start", "eta_sin2_x_trip_start",
    "eta_cos2_x_trip_start", "eta_sin3_x_trip_start", "eta_cos3_x_trip_start",
    "dist_sin1_x_trip_end", "dist_cos1_x_trip_end", "dist_sin2_x_trip_end",
    "dist_cos2_x_trip_end", "dist_sin3_x_trip_end", "dist_cos3_x_trip_end",
    "speed_sin1_x_trip_end", "speed_cos1_x_trip_end", "speed_sin2_x_trip_end",
    "speed_cos2_x_trip_end", "speed_sin3_x_trip_end", "speed_cos3_x_trip_end",
    "eta_sin1_x_trip_end", "eta_cos1_x_trip_end", "eta_sin2_x_trip_end",
    "eta_cos2_x_trip_end", "eta_sin3_x_trip_end", "eta_cos3_x_trip_end",
    "dist_sin1_x_weekday", "dist_cos1_x_weekday", "dist_sin2_x_weekday",
    "dist_cos2_x_weekday", "dist_sin3_x_weekday", "dist_cos3_x_weekday",
    "speed_sin1_x_weekday", "speed_cos1_x_weekday", "speed_sin2_x_weekday",
    "speed_cos2_x_weekday", "speed_sin3_x_weekday", "speed_cos3_x_weekday",
    "eta_sin1_x_weekday", "eta_cos1_x_weekday", "eta_sin2_x_weekday",
    "eta_cos2_x_weekday", "eta_sin3_x_weekday", "eta_cos3_x_weekday",
    "speed_tau", "eta_tau", "dist_tau"
    )

mcmc_posterior = coda.samples( jags_model, 
    variable.names = varnames,
    n.iter = 1e5, thin = 50,  )
mcmc_posterior = mcmc_posterior[ -(1:100) ]
mcm_names = names( mcmc_posterior )

save( mcmc_posterior,
file = "G:/azure_hackathon/datasets2/bayes_testing/full_data_posterior.RData" )

# load( "G:/azure_hackathon/datasets2/bayes_testing/test_posterior.RData" )

# trip_summary[ , c("sin1", "cos1", "sin2", "cos2", "sin3", "cos3") := {
#     list( 
#         sin( 2 * pi * 1 * hour / 24 ), cos( 2 * pi * 1 * hour / 24 ),
#         sin( 2 * pi * 2 * hour / 24 ), cos( 2 * pi * 2 * hour / 24 ),
#         sin( 2 * pi * 3 * hour / 24 ), cos( 2 * pi * 3 * hour / 24 )
#     )
# } ]
# trip_summary[ , log_path_dist := log(path_dist) ]
# trip_summary[ , log_mean_speed := log(mean_speed) ]
# trip_summary[ , log_timediff := log(timediff) ]
# 
# numeric_means = lapply( trip_summary, function(x){
#     if ( !is.factor(x) ){
#         return( mean(x) )
#     } else{
#         NULL
#     }
# } )
# 
# dataset1$timediff[2]
# 
# predict_data = dataset1[ 1, list(azure_dist, OSRM_dist, hour, weekday,
#     trip_start, trip_end, rush_hour, historic_log_var_speed) ]
# predict_data
# 
# log( dataset1[ 1, list(path_dist) ] ) - numeric_means$log_path_dist

# dataset1 = trip_summary[ 1:1000 ]
# dataset1[ , c("sin1", "cos1", "sin2", "cos2", "sin3", "cos3") := {
#     list( 
#         sin( 2 * pi * 1 * hour / 24 ), cos( 2 * pi * 1 * hour / 24 ),
#         sin( 2 * pi * 2 * hour / 24 ), cos( 2 * pi * 2 * hour / 24 ),
#         sin( 2 * pi * 3 * hour / 24 ), cos( 2 * pi * 3 * hour / 24 )
#     )
# } ]
# 
# dataset1[ , log_path_dist := log(path_dist) ]
# dataset1[ , log_mean_speed := log(mean_speed) ]
# dataset1[ , log_timediff := log(timediff) ]
# 
# 
# timediffs = dataset1$timediff
# 
# factor_vars = dataset1[ , list( weekday, rush_hour, trip_start, trip_end ) ]
# numeric_x = dataset1[ , mget( setdiff( names( dataset1 ), names(factor_vars ) ) ) ]
# numeric_x[ , c("trj_id", "timediff") := NULL ]
# numeric_x[ , c("sampling_rate", "log_sampling_rate_var", "historic_crow_dist",
#     "historic_path_dist", "historic_mean_speed", "mean_speed",
#     "path_dist", "path_dist2", "log_var_speed", "log_timediff",
#     "hour") := NULL ]
# numeric_x[ , c("start_x", "end_y") := NULL ]
# 
# numeric_x[ , c("crow_dist", "start_y", "end_x",
#     "historic_timediff", "crow_dist.2", "historic_sampling_rate") := NULL ]
# 
# log_timediffs = log( timediffs )
# 
# numeric_x = scale( numeric_x, scale = FALSE )
# scale_means = attr( numeric_x, "scaled:center")
# 
# jags_data = cbind( as.data.table(numeric_x), factor_vars,
#     log_timediff = log_timediffs )

aa = jags_data[1]

list2env( aa, envir = globalenv() )

a = bayesian_predict( azure_dist, OSRM_dist, hour, weekday, trip_start, trip_end, 
    rush_hour, historic_log_var_speed, numeric_means )

jags_data[ , id := 1:.N ]

pred = jags_data[ , {
    # pred_ = Inf
    # while( pred_ > 3000 ){
        pred_ = bayesian_predict( azure_dist, OSRM_dist, hour, weekday, trip_start, trip_end, 
            rush_hour, historic_log_var_speed )
        # pred_quants = quantile( pred_, c(0.45, 0.55) )
    # } 
    median(pred_)
}, by = id ]

sqrt( mean( ( data.table( timediffs, pred$V1 )[ , timediffs - V2] )^2 ) )

plot( data.table( timediffs, pred$V1 ) )
which( pred$V1 > 1e10 )

data.table( timediffs, pred$V1 )[ , which(V2 > 5e30) ]

aa = jags_data[923]
list2env( aa, envir = globalenv() )

bayesian_predict = function( azure_dist, OSRM_dist, hour, weekday,
    trip_start, trip_end, rush_hour, historic_log_var_speed ){
    
    n = nrow(mcmc_posterior)
    
    trip_start_num = as.numeric( trip_start )
    trip_end_num = as.numeric( trip_end )
    weekday_num = as.numeric( weekday )
    rush_hour_num = as.numeric( rush_hour )
    
    path_dist_mu =
        azure_dist * mcmc_posterior$`dist_betas[1]` +
        azure_dist.2 * mcmc_posterior$`dist_betas[2]` +
        OSRM_dist * mcmc_posterior$`dist_betas[3]` +
        OSRM_dist.2 * mcmc_posterior$`dist_betas[4]` +
        sin1 * mcmc_posterior$`dist_betas[5]` +
        cos1 * mcmc_posterior$`dist_betas[6]` +
        sin2 * mcmc_posterior$`dist_betas[7]` +
        cos2 * mcmc_posterior$`dist_betas[8]` +
        sin3 * mcmc_posterior$`dist_betas[9]` +
        cos3 * mcmc_posterior$`dist_betas[10]` +
        mcmc_posterior[[ paste0( "dist_weekday_betas[", weekday_num, "]" ) ]] +
        mcmc_posterior[[ paste0( "dist_sin1_x_trip_start[", trip_start_num, "]" ) ]] * sin1 +
        mcmc_posterior[[ paste0( "dist_cos1_x_trip_start[", trip_start_num, "]" ) ]] * cos1 +
        mcmc_posterior[[ paste0( "dist_sin2_x_trip_start[", trip_start_num, "]" ) ]] * sin2 +
        mcmc_posterior[[ paste0( "dist_cos2_x_trip_start[", trip_start_num, "]" ) ]] * cos2 +
        mcmc_posterior[[ paste0( "dist_sin3_x_trip_start[", trip_start_num, "]" ) ]] * sin3 +
        mcmc_posterior[[ paste0( "dist_cos3_x_trip_start[", trip_start_num, "]" ) ]] * cos3 +
        mcmc_posterior[[ paste0( "dist_sin1_x_trip_end[", trip_end_num, "]" ) ]] * sin1 +
        mcmc_posterior[[ paste0( "dist_cos1_x_trip_end[", trip_end_num, "]" ) ]] * cos1 +
        mcmc_posterior[[ paste0( "dist_sin2_x_trip_end[", trip_end_num, "]" ) ]] * sin2 +
        mcmc_posterior[[ paste0( "dist_cos2_x_trip_end[", trip_end_num, "]" ) ]] * cos2 +
        mcmc_posterior[[ paste0( "dist_sin3_x_trip_end[", trip_end_num, "]" ) ]] * sin3 +
        mcmc_posterior[[ paste0( "dist_cos3_x_trip_end[", trip_end_num, "]" ) ]] * cos3
    
    # path_dist_mu = cbind(
    #     azure_dist * mcmc_posterior$`dist_betas[1]`,
    #     azure_dist.2 * mcmc_posterior$`dist_betas[2]`,
    #     OSRM_dist * mcmc_posterior$`dist_betas[3]`,
    #     OSRM_dist.2 * mcmc_posterior$`dist_betas[4]`,
    #     sin1 * mcmc_posterior$`dist_betas[5]`,
    #     cos1 * mcmc_posterior$`dist_betas[6]`,
    #     sin2 * mcmc_posterior$`dist_betas[7]`,
    #     cos2 * mcmc_posterior$`dist_betas[8]`,
    #     sin3 * mcmc_posterior$`dist_betas[9]`,
    #     cos3 * mcmc_posterior$`dist_betas[10]`,
    #     mcmc_posterior[[ paste0( "dist_weekday_betas[", weekday_num, "]" ) ]],
    #     mcmc_posterior[[ paste0( "dist_sin1_x_trip_start[", trip_start_num, "]" ) ]] * sin1,
    #     mcmc_posterior[[ paste0( "dist_cos1_x_trip_start[", trip_start_num, "]" ) ]] * cos1,
    #     mcmc_posterior[[ paste0( "dist_sin2_x_trip_start[", trip_start_num, "]" ) ]] * sin2,
    #     mcmc_posterior[[ paste0( "dist_cos2_x_trip_start[", trip_start_num, "]" ) ]] * cos2,
    #     mcmc_posterior[[ paste0( "dist_sin3_x_trip_start[", trip_start_num, "]" ) ]] * sin3,
    #     mcmc_posterior[[ paste0( "dist_cos3_x_trip_start[", trip_start_num, "]" ) ]] * cos3,
    #     mcmc_posterior[[ paste0( "dist_sin1_x_trip_end[", trip_end_num, "]" ) ]] * sin1,
    #     mcmc_posterior[[ paste0( "dist_cos1_x_trip_end[", trip_end_num, "]" ) ]] * cos1,
    #     mcmc_posterior[[ paste0( "dist_sin2_x_trip_end[", trip_end_num, "]" ) ]] * sin2,
    #     mcmc_posterior[[ paste0( "dist_cos2_x_trip_end[", trip_end_num, "]" ) ]] * cos2,
    #     mcmc_posterior[[ paste0( "dist_sin3_x_trip_end[", trip_end_num, "]" ) ]] * sin3,
    #     mcmc_posterior[[ paste0( "dist_cos3_x_trip_end[", trip_end_num, "]" ) ]] * cos3 )
    # path_dist_mu = matrixStats::rowLogSumExps(path_dist_mu)


        dist_tau = mcmc_posterior$dist_tau
        log_path_dist = rnorm( n, path_dist_mu, dist_tau^(-1/2) )
        print( mean( log_path_dist ) )
        
        mean_speed_mu =
            mcmc_posterior$`speed_betas[1]` +
            OSRM_dist *  mcmc_posterior$`speed_betas[2]` +
            OSRM_dist.2 *  mcmc_posterior$`speed_betas[3]` +
            historic_log_var_speed *  mcmc_posterior$`speed_betas[4]` +
            log_path_dist *  mcmc_posterior$`speed_betas[5]` +
            sin1 *  mcmc_posterior$`speed_betas[6]` +
            cos1 *  mcmc_posterior$`speed_betas[7]` +
            sin2 *  mcmc_posterior$`speed_betas[8]` +
            cos2 *  mcmc_posterior$`speed_betas[9]` +
            sin3 *  mcmc_posterior$`speed_betas[10]` +
            cos3 *  mcmc_posterior$`speed_betas[11]` +
            exp(log_path_dist) * mcmc_posterior$`speed_betas[12]` +
            mcmc_posterior[[ paste0( "speed_weekday_betas[", weekday_num, "]" ) ]] +
            mcmc_posterior[[ paste0( "speed_rush_betas[", as.numeric(rush_hour), "]" ) ]] +
            mcmc_posterior[[ paste0( "speed_tripstart_betas[", trip_start_num, "]" ) ]] +
            mcmc_posterior[[ paste0( "speed_tripend_betas[", trip_start_num, "]" ) ]] +
            mcmc_posterior[[ paste0( "speed_sin2_x_trip_start[", trip_start_num, "]" ) ]] * sin2 +
            mcmc_posterior[[ paste0( "speed_sin3_x_trip_start[", trip_start_num, "]" ) ]] * sin3 +
            mcmc_posterior[[ paste0( "speed_cos1_x_trip_end[", trip_end_num, "]" ) ]] * cos1 +
            mcmc_posterior[[ paste0( "speed_sin3_x_trip_end[", trip_end_num, "]" ) ]] * sin3 +
            mcmc_posterior[[ paste0( "speed_cos3_x_trip_end[", trip_end_num, "]" ) ]] * cos3 +
            mcmc_posterior[[ paste0( "speed_sin1_x_weekday[", weekday_num, "]" ) ]] * sin1 +
            mcmc_posterior[[ paste0( "speed_cos3_x_weekday[", weekday_num, "]" ) ]] * cos3 +
            mcmc_posterior[[ paste0( "speed_sin1_x_trip_start[", trip_end_num, "]" ) ]] * sin1 +
            mcmc_posterior[[ paste0( "speed_cos1_x_trip_start[", trip_end_num, "]" ) ]] * cos1 +
            mcmc_posterior[[ paste0( "speed_sin1_x_trip_end[", trip_end_num, "]" ) ]] * sin1 +
            mcmc_posterior[[ paste0( "speed_cos2_x_trip_end[", trip_end_num, "]" ) ]] * cos2 +
            mcmc_posterior[[ paste0( "speed_sin3_x_weekday[", weekday_num, "]" ) ]] * sin3
        # mean_speed_mu = cbind(
        #     mcmc_posterior$`speed_betas[1]` ,
        #     OSRM_dist *  mcmc_posterior$`speed_betas[2]` ,
        #     OSRM_dist.2 *  mcmc_posterior$`speed_betas[3]` ,
        #     historic_log_var_speed *  mcmc_posterior$`speed_betas[4]` ,
        #     log_path_dist *  mcmc_posterior$`speed_betas[5]` ,
        #     sin1 *  mcmc_posterior$`speed_betas[6]` ,
        #     cos1 *  mcmc_posterior$`speed_betas[7]` ,
        #     sin2 *  mcmc_posterior$`speed_betas[8]` ,
        #     cos2 *  mcmc_posterior$`speed_betas[9]` ,
        #     sin3 *  mcmc_posterior$`speed_betas[10]` ,
        #     cos3 *  mcmc_posterior$`speed_betas[11]` ,
        #     exp(log_path_dist) * mcmc_posterior$`speed_betas[12]` ,
        #     mcmc_posterior[[ paste0( "speed_weekday_betas[", weekday_num, "]" ) ]] ,
        #     mcmc_posterior[[ paste0( "speed_rush_betas[", as.numeric(rush_hour), "]" ) ]] ,
        #     mcmc_posterior[[ paste0( "speed_tripstart_betas[", trip_start_num, "]" ) ]] ,
        #     mcmc_posterior[[ paste0( "speed_tripend_betas[", trip_start_num, "]" ) ]] ,
        #     mcmc_posterior[[ paste0( "speed_sin2_x_trip_start[", trip_start_num, "]" ) ]] * sin2 ,
        #     mcmc_posterior[[ paste0( "speed_sin3_x_trip_start[", trip_start_num, "]" ) ]] * sin3 ,
        #     mcmc_posterior[[ paste0( "speed_cos1_x_trip_end[", trip_end_num, "]" ) ]] * cos1 ,
        #     mcmc_posterior[[ paste0( "speed_sin3_x_trip_end[", trip_end_num, "]" ) ]] * sin3 ,
        #     mcmc_posterior[[ paste0( "speed_cos3_x_trip_end[", trip_end_num, "]" ) ]] * cos3 ,
        #     mcmc_posterior[[ paste0( "speed_sin1_x_weekday[", weekday_num, "]" ) ]] * sin1 ,
        #     mcmc_posterior[[ paste0( "speed_cos3_x_weekday[", weekday_num, "]" ) ]] * cos3 ,
        #     mcmc_posterior[[ paste0( "speed_sin1_x_trip_start[", trip_end_num, "]" ) ]] * sin1 ,
        #     mcmc_posterior[[ paste0( "speed_cos1_x_trip_start[", trip_end_num, "]" ) ]] * cos1 ,
        #     mcmc_posterior[[ paste0( "speed_sin1_x_trip_end[", trip_end_num, "]" ) ]] * sin1 ,
        #     mcmc_posterior[[ paste0( "speed_cos2_x_trip_end[", trip_end_num, "]" ) ]] * cos2 ,
        #     mcmc_posterior[[ paste0( "speed_sin3_x_weekday[", weekday_num, "]" ) ]] * sin3 )
        # mean_speed_mu = matrixStats::rowLogSumExps(mean_speed_mu)
        
        speed_tau = mcmc_posterior$speed_tau
        log_mean_speed  = rnorm( n, mean_speed_mu, speed_tau^(-1/2) )
                print( mean( log_mean_speed ) )

                
        timediff_mu =
            mcmc_posterior$`eta_betas[1]` +
            OSRM_dist * mcmc_posterior$`eta_betas[2]` +
            OSRM_dist.2 * mcmc_posterior$`eta_betas[3]` +
            mcmc_posterior[[ paste0( "eta_rush_betas[", rush_hour_num, "]" ) ]] +
            mcmc_posterior[[ paste0( "eta_weekday_betas[", weekday_num, "]" ) ]] +
            mcmc_posterior[[ paste0( "eta_tripstart_betas[", trip_start_num, "]" ) ]] +
            mcmc_posterior[[ paste0( "eta_tripend_betas[", trip_end_num, "]" ) ]] +
            log_path_dist * mcmc_posterior$`eta_betas[4]` +
            log_mean_speed * mcmc_posterior$`eta_betas[5]` +
            log_path_dist / log_mean_speed * mcmc_posterior$`eta_betas[6]` +
            (log_path_dist - log_mean_speed) * mcmc_posterior$`eta_betas[7]` +
            sin1 * mcmc_posterior$`eta_betas[8]` +
            cos1 * mcmc_posterior$`eta_betas[9]` +
            sin2 * mcmc_posterior$`eta_betas[10]` +
            cos2 * mcmc_posterior$`eta_betas[11]` +
            sin3 * mcmc_posterior$`eta_betas[12]` +
            cos3 * mcmc_posterior$`eta_betas[13]` +
            mcmc_posterior[[ paste0( "eta_sin2_x_weekday[", weekday_num, "]" ) ]] * sin2
            #             timediff_mu = cbind(
            # mcmc_posterior$`eta_betas[1]` ,
            # OSRM_dist * mcmc_posterior$`eta_betas[2]` ,
            # OSRM_dist.2 * mcmc_posterior$`eta_betas[3]` ,
            # mcmc_posterior[[ paste0( "eta_rush_betas[", rush_hour_num, "]" ) ]] ,
            # mcmc_posterior[[ paste0( "eta_weekday_betas[", weekday_num, "]" ) ]] ,
            # mcmc_posterior[[ paste0( "eta_tripstart_betas[", trip_start_num, "]" ) ]] ,
            # mcmc_posterior[[ paste0( "eta_tripend_betas[", trip_end_num, "]" ) ]] ,
            # log_path_dist * mcmc_posterior$`eta_betas[4]` ,
            # log_mean_speed * mcmc_posterior$`eta_betas[5]` ,
            # log_path_dist / log_mean_speed * mcmc_posterior$`eta_betas[6]` ,
            # (log_path_dist - log_mean_speed) * mcmc_posterior$`eta_betas[7]` ,
            # sin1 * mcmc_posterior$`eta_betas[8]` , 
            # cos1 * mcmc_posterior$`eta_betas[9]` ,
            # sin2 * mcmc_posterior$`eta_betas[10]` ,
            # cos2 * mcmc_posterior$`eta_betas[11]` ,
            # sin3 * mcmc_posterior$`eta_betas[12]` ,
            # cos3 * mcmc_posterior$`eta_betas[13]` ,
            # mcmc_posterior[[ paste0( "eta_sin2_x_weekday[", weekday_num, "]" ) ]] * sin2 )
            #             
            # timediff_mu = matrixStats::rowLogSumExps(timediff_mu)
                        
        eta_tau = mcmc_posterior$eta_tau
        log_timediff = rnorm( n, timediff_mu, eta_tau^(-1/2) )
        
        ( exp( log_timediff ) )
}

        
        
        
        
        
        
        
        
# # plot( mcmc_posterior$`speed_betas[5]`, type = "l" )
# 
# par( mfrow = c(1, 3 ) )
# 
# 
# pred_vars = c( "log_timediff", "log_mean_speed", "log_path_dist" )
# for ( v in pred_vars ){
#     get_vars = mcm_names[ grepl( v, mcm_names ) ]
#     scalingx = 0
#     if ( v != "log_timediff" ){
#         scalingx = scale_means[v]
#     }
#     pred_dist = colMeans( mcmc_posterior[ , mget( get_vars ) ] + scalingx )
#     plot( pred_dist, dataset1[[v]], main = v )
#     abline( 0, 1, col = "red" )
#     x = r_squared( exp(dataset1[[v]]), exp(pred_dist) )
#     # x = sqrt( mean( (exp(dataset1[[v]]) - exp(pred_dist))^2 ) )
#     print(x)
# }
# 
# betas = mcm_names[ grepl( "betas", mcm_names ) ]
# par ( mfrow = c(5, 5 ) )
# for ( b in betas ){
#     plot( mcmc_posterior[[b]], type = "l",
#         main = b )
#     abline( 0, 0, col = "red" )
# }
# 
# path_dist_betas = mcm_names[ grepl( "dist_", mcm_names ) ]
# path_dist_betas = path_dist_betas[ ! grepl( "pred", path_dist_betas ) ]
# par( mfrow = c(5,5) )
# for ( b in path_dist_betas ){
#     if ( !is.null(mcmc_posterior[[b]] ) ){
#         hist( mcmc_posterior[[b]], main = b, prob = T, breaks = 50 )
#         lines( density( mcmc_posterior$`dist_betas[13]` ), col = "red" )
#         abline( v = 0, col = "red" )
#         x = round( mean( mcmc_posterior[[b]] <= 0 ), 3 )
#         legend( x = "topright", legend = x )
#     }
# }
# 
# path_dist_betas = mcm_names[ grepl( "speed_", mcm_names ) ]
# path_dist_betas = path_dist_betas[ ! grepl( "pred", path_dist_betas ) ]
# par( mfrow = c(5,5) )
# for ( b in path_dist_betas ){
#     hist( mcmc_posterior[[b]], main = b, prob = T, breaks = 50 )
#     lines( density( mcmc_posterior$`speed_betas[14]` ), col = "red" )
#     abline( v = 0, col = "red" )
#         x = round( mean( mcmc_posterior[[b]] <= 0 ), 3 )    
#         legend( x = "topright", legend = x )
# }
# 
# 
# path_dist_betas = mcm_names[ grepl( "eta_", mcm_names ) ]
# path_dist_betas = path_dist_betas[ ! grepl( "pred", path_dist_betas ) ]
# par( mfrow = c(5,5) )
# for ( b in path_dist_betas ){
#     hist( mcmc_posterior[[b]], main = b, prob = T, breaks = 50 )
#     lines( density( mcmc_posterior$`eta_betas[20]` ), col = "red" )
#     abline( v = 0, col = "red" )
#         x = round( mean( mcmc_posterior[[b]] <= 0 ), 3 )
#         legend( x = "topright", legend = x )
# }
# 
# 
# 
# r_squared = function( y, yhat ){
#     1 - sum( ( y - yhat )^2 ) / sum( ( y-mean(y))^2 )
# }


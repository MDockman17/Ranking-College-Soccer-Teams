update_ratings = function(result,prior_rating,opponent_prior_rating,prior_volatility, opp_prior_volatility){
  
  ratings_diff = prior_rating - opponent_prior_rating
  
  sd = sqrt(prior_volatility^2 + opp_prior_volatility^2)
  
  if(result == 1){
    
    gradient = (1/sd)*(dnorm(ratings_diff - 1,sd = sd)/pnorm(ratings_diff - 1,sd = sd))}
  
  if(result == 0){
    
    gradient = -(1/sd)*(dnorm(-ratings_diff - 1,sd = sd)/(pnorm(-ratings_diff - 1,sd = sd)))}
  
  if(result == 0.5){
    
    gradient = (-(1/sd)*dnorm(1 - ratings_diff,sd = sd) + (1/sd)*dnorm(-1 - ratings_diff,sd = sd))/((pnorm(1 - ratings_diff,sd = sd) - pnorm(-1 - ratings_diff,sd = sd)))
    
  }
  
  return(gradient) 
  
}

update_volatility = function(result,prior_rating,opp_prior_rating,prior_volatility, opp_prior_volatility){
  
  ratings_diff = prior_rating - opp_prior_rating
  
  sd = sqrt(prior_volatility^2 + opp_prior_volatility^2)
  
  if(result == 1){
    
    gradient = -(dnorm(ratings_diff - 1,sd = sd)/pnorm(ratings_diff - 1,sd = sd))*((ratings_diff-1)*prior_volatility/(sd^3))
  }
  
  if(result == 0){
    
    gradient = -(dnorm(-ratings_diff - 1,sd = sd)/pnorm(-ratings_diff - 1,sd = sd))*((-ratings_diff - 1)*prior_volatility/(sd^3))
  }
  
  if(result == 0.5){
    
    gradient = ((-dnorm(1 - ratings_diff, sd=sd)*(1 - ratings_diff)*prior_volatility/(sd^3)) + dnorm(-1 - ratings_diff, sd=sd)*(-1 - ratings_diff)*prior_volatility/(sd^3))/(pnorm(1 - ratings_diff,sd=sd) - pnorm(-1 - ratings_diff, sd=sd))
    
  }
  
  return(gradient) 
  
}

select_variables = function(data, threshold){
  
  df = data
  
  n = nrow(df)
  
  p = 22
  
  Data_bas = bas.lm(goal_differential ~ corners     + fouls_won  + goal_kicks + offsides + shots +     shots_on_target_percentage + shots_on_wood_work     + shots_saved + throw_ins + save_percentage +      red_cards + opp_corners + opp_fouls_won  +         opp_goal_kicks + opp_offsides + opp_shots +        opp_shots_on_target_percentage +                   opp_shots_on_wood_work + opp_shots_saved +         opp_throw_ins + opp_save_percentage +              opp_red_cards , data = df,                         prior= "g-prior",alpha= n,
                    n.models= 2^p, initprobs= "Uniform")
  
  summary = data.frame(summary(Data_bas))[2:(p+1),]
  
  variables = row.names(summary)[summary[,1] > threshold]
  
  return(variables)
  
}

adjust_goal_differential = function(train, test, threshold){
  
  set.seed(17)
  
  variables = select_variables(train, threshold)
  
  n_train = nrow(train)
  
  n_test = nrow(test)
  
  prop = n_train/(n_train + n_test)
  
  data_gd = bind_rows(train,test)
  
  split = initial_time_split(data_gd[,c("goal_differential",variables)], prop = prop)
  train = training(split)
  test = testing(split)
  
  folds =  vfold_cv(train)
  
  rf_model = rand_forest(mtry = tune(), min_n = tune(), trees = 250) %>% 
    set_engine("ranger", importance = "impurity", num.threads = 4) %>% 
    set_mode("regression")
  
  rf_workflow = workflow() %>%
    add_model(rf_model) %>%
    add_formula(goal_differential ~ .)
  
  rf_final = rf_workflow %>% 
    tune_grid(
      resamples = folds,
      grid = 25,
      control = control_grid(save_pred = TRUE),
      metrics = metric_set(rmse)
    ) %>%
    select_best("rmse")
  
  final_wf = rf_workflow %>%
    finalize_workflow(rf_final) %>%
    last_fit(split)
  
  preds_rf = final_wf %>%
    collect_predictions()
  
  return(preds_rf$.pred)
  
}

run_goal_diff_model = function(data, threshold, league){
  
  train  = data.frame()
  
  if(league == "womens"){
    lower = 1
    upper = (length(data)/2) - 1
  }
  else{
    lower = (length(data)/2) + 1
    upper = length(data) - 1
  }
  
  for(i in lower:upper){
    
    if(i == lower + 1){
      
      train$pred_goal_differential = 0
      data[[i-1]]$pred_goal_differential = 0
      train$avg_pred_goal_diff = 0
      data[[i-1]]$avg_pred_goal_diff = 0
    }
    
    train = rbind(train, data[[i]])
    
    test = data[[i+1]]
    
    pred_goal_diff = adjust_goal_differential(train,test,threshold)
    
    if(length(pred_goal_diff) > nrow(test)){
      
      pred_goal_diff = tail(pred_goal_diff, n = nrow(test))
      
    }
    
    data[[i+1]]$pred_goal_differential = pred_goal_diff
    
    pred_goal_diff_df = data[[i+1]] %>%
      group_by(game_id) %>%
      summarise(avg_pred_goal_diff = mean(abs(pred_goal_differential)))
    
    data[[i+1]] = left_join(data[[i+1]], pred_goal_diff_df, by = "game_id")
    
    data[[i+1]] = data[[i+1]] %>%
      mutate(pred_goal_differential = sign(pred_goal_differential)*avg_pred_goal_diff)
    
    
  }
  
  return(data)
  
}

fix_differing_gds = function(data){
  
  for(i in 1:length(data)){
    
    df = data[[i]]
    
    for(j in 1:nrow(df)){
      
      if(sign(df$goal_differential[j]) != sign(df$pred_goal_differential[j])){
        
        df$pred_goal_differential[j] = sign(df$goal_differential[j])*0.5
        
      }
    }
    
    data[[i]] = df
  }
  
  return(data)
}


create_ratings = function(data, hyperparameters, last_year_ratings = NULL, last_year_vol = NULL){
  
  df = data
  
  season_year = df$season_year[1]
  
  initial_vol = hyperparameters[1]
  vol_param = hyperparameters[2]
  he_param = hyperparameters[3]
  gd_param = hyperparameters[4]
  sd_param = hyperparameters[5]
  update_param = hyperparameters[6]
  df$rating = 0
  df$prior_rating = 0
  df$opp_prior_rating = 0
  df$win_probability = 0
  df$draw_probability = 0
  df$loss_probability = 0
  df$offensive_rating = 1
  df$defensive_rating = 1
  df$prior_off_rating = 1
  df$prior_def_rating = 1
  df$opp_prior_off_rating = 1
  df$opp_prior_def_rating = 1
  df$update_size = 0
  df$prior_update_size = 0
  df$volatility = initial_vol
  df$prior_volatility = initial_vol
  df$opp_prior_volatility = initial_vol
  df$volatility_update_size = 0
  
  for(j in 1:nrow(df)){
    
    team = df$team_market.x[j]
    
    game_number = df$team_game_number[j]
    
    opponent = df$team_market[j] 
    
    opponent_game_number =   df$opponent_team_game_number[j]
    
    if(game_number > 1){
      
      indices = which(df$team_market.x == df$team_market.x[j])
      
      index = indices[as.integer(game_number-1)]
      
      prior_rating = df$rating[index]
      
      df$prior_rating[j] = prior_rating
      
      prior_volatility = df$volatility[index]
      
      df$prior_volatility[j] = prior_volatility
      
      prior_update_size = df$update_size[index]
      
      df$prior_update_size[j] = prior_update_size
      
      prior_offensive_rating = df$offensive_rating[index]
      
      df$prior_off_rating[j] = prior_offensive_rating
      
      prior_defensive_rating = df$defensive_rating[index]
      
      df$prior_def_rating[j] = prior_defensive_rating
    }
    
    
    
    if(game_number == 1 & is.null(last_year_ratings)){
      prior_rating = 0
      prior_volatility = initial_vol
      prior_update_size = 0
      prior_offensive_rating = 1
      prior_defensive_rating = 1
    }
    
    if(game_number == 1 & !is.null(last_year_ratings)){
      index_rating = which(last_year_ratings$team_market.x == team)
      index_vol = which(last_year_vol$team_market.x == team)
      if(length(index_rating)==0){
        prior_rating = 0
        prior_volatility = initial_vol
      }
      else{
        prior_rating = last_year_ratings$final_rating[index_rating]
        df$prior_rating[j] = prior_rating
        prior_volatility = last_year_vol$final_volatility[index_vol]
        df$prior_volatility[j] = prior_volatility
      }
      prior_offensive_rating = 1
      prior_defensive_rating = 1
      prior_update_size = 0}
    
    if(opponent_game_number > 1){
      
      opponent_indices = which(df$team_market.x == df$team_market[j])
      
      opponent_index = opponent_indices[as.integer(opponent_game_number-1)]
      
      opponent_prior_rating = df$rating[opponent_index]
      
      df$opp_prior_rating[j] = opponent_prior_rating
      
      opponent_prior_volatility = df$volatility[opponent_index]
      
      df$opp_prior_volatility[j] = opponent_prior_volatility
      
      opponent_prior_offensive_rating = df$offensive_rating[opponent_index]
      
      opponent_prior_defensive_rating = df$defensive_rating[opponent_index]
      
      df$opp_prior_off_rating[j] = opponent_prior_offensive_rating
      
      df$opp_prior_def_rating[j] = opponent_prior_defensive_rating
      
    }
    
    if(opponent_game_number == 1 & is.null(last_year_ratings)){
      opponent_prior_rating = 0
      opponent_prior_volatility = initial_vol
      opponent_prior_offensive_rating = 1
      opponent_prior_defensive_rating = 1
    }
    
    if(opponent_game_number == 1 & !is.null(last_year_ratings)){
      opponent_index_rating = which(last_year_ratings$team_market.x == opponent)
      opponent_index_vol = which(last_year_vol$team_market.x == opponent)
      if(length(opponent_index_rating)==0){
        opponent_prior_rating = 0
        opponent_prior_volatility = initial_vol
      }
      else{
        opponent_prior_rating = last_year_ratings$final_rating[opponent_index_rating]
        df$opp_prior_rating[j] = opponent_prior_rating
        opponent_prior_volatility = last_year_vol$final_volatility[opponent_index_vol]
        df$opp_prior_volatility[j] = opponent_prior_volatility
      }
      
      opponent_prior_offensive_rating = 1
      opponent_prior_defensive_rating = 1}
    
    
    result = df$result[j]
    
    sd = sqrt(prior_volatility^2 + opponent_prior_volatility^2)
    
    if(df$location_type[j]=="HOME"){
      
      win_probability = pnorm(prior_rating+he_param - opponent_prior_rating - 1 , sd = sd)
      
      loss_probability = pnorm(-(prior_rating+he_param - opponent_prior_rating) - 1 , sd = sd)
      
      draw_probability = pnorm(1 - (prior_rating + he_param - opponent_prior_rating), sd = sd) - pnorm(-1 - (prior_rating + he_param - opponent_prior_rating), sd = sd)
      
      grad = update_ratings(result,prior_rating+he_param,opponent_prior_rating, prior_volatility,opponent_prior_volatility)
      
      volatility_grad = update_volatility(result,prior_rating+he_param,opponent_prior_rating, prior_volatility,opponent_prior_volatility)
    }
    
    if(df$location_type[j] == "AWAY"){
      
      win_probability = pnorm(prior_rating - he_param - opponent_prior_rating - 1 , sd = sd)
      
      loss_probability = pnorm(-(prior_rating - he_param - opponent_prior_rating) - 1 , sd = sd)
      
      draw_probability = pnorm(1 - (prior_rating - he_param - opponent_prior_rating), sd = sd) - pnorm(-1 - (prior_rating - he_param - opponent_prior_rating), sd = sd)
      
      grad = update_ratings(result,prior_rating,opponent_prior_rating+he_param, prior_volatility,opponent_prior_volatility)
      
      volatility_grad = update_volatility(result,prior_rating,opponent_prior_rating+he_param, prior_volatility,opponent_prior_volatility)
    }
    
    
    if(df$location_type[j] == "NEUTRAL"){
      
      win_probability = pnorm(prior_rating - opponent_prior_rating - 1 , sd = sd)
      
      loss_probability = pnorm(-(prior_rating - opponent_prior_rating) - 1 , sd = sd)
      
      draw_probability = pnorm(1 - (prior_rating  - opponent_prior_rating), sd = sd) - pnorm(-1 - (prior_rating  - opponent_prior_rating), sd = sd)
      
      grad = update_ratings(result,prior_rating,opponent_prior_rating,prior_volatility,opponent_prior_volatility)
      
      volatility_grad = update_volatility(result,prior_rating,opponent_prior_rating, prior_volatility,opponent_prior_volatility)
    }
    
    df$win_probability[j] = win_probability
    df$draw_probability[j] = draw_probability
    df$loss_probability[j] = loss_probability
    
    if(abs(df$goal_differential[j])<=1 & is.null(last_year_ratings)){
      goal_diff = abs(df$goal_differential[j])}
    
    if(abs(df$goal_differential[j])>1 & is.null(last_year_ratings)){
      goal_diff = sqrt(abs(df$goal_differential[j]))
    }
    
    if(abs(df$pred_goal_differential[j])<=1 & !is.null(last_year_ratings)){
      goal_diff = abs(df$pred_goal_differential[j])}
    
    if(abs(df$pred_goal_differential[j])>1 & !is.null(last_year_ratings)){
      goal_diff = sqrt(abs(df$pred_goal_differential[j]))
    }
    
    shots_diff = sqrt(abs(df$diff_in_shots[j]))
    
    if(goal_diff != 0){
      update = (gd_param*goal_diff + sd_param*shots_diff)*grad + update_param*prior_update_size
    }
    
    if(goal_diff == 0){
      update = (gd_param*0.5 + sd_param*shots_diff)*grad + update_param*prior_update_size 
    }
    
    if(update < 0){
      
      update_size = max(c(update,-1))
      
      df$rating[j] = prior_rating +
        update_size
    }
    
    if(update >= 0){
      
      update_size = min(c(update,1))
      
      df$rating[j] = prior_rating +
        update_size
    }
    
    df$update_size[j] = update_size
    
    volatility_update_size = vol_param*volatility_grad
    
    df$volatility_update_size[j] = volatility_update_size
    
    df$volatility[j] = prior_volatility + volatility_update_size
    
    goals_scored = df$goals[j]
    
    goals_allowed = df$opp_goals[j]
    
    offensive_update = (sqrt(prior_offensive_rating* opponent_prior_defensive_rating) + goals_scored)/2
    
    df$offensive_rating[j] = offensive_update
    
    defensive_update= (sqrt(prior_defensive_rating*opponent_prior_offensive_rating) + goals_allowed)/2
    
    df$defensive_rating[j] = defensive_update
    
  }
  return(df) 
  
}

find_final_ratings = function(data){
  
  final_ratings = data %>%
    group_by(team_market.x) %>%
    summarise(n=n())
  
  final_ratings$final_rating = 0
  
  for(j in 1:nrow(final_ratings)){
    for(i in 1:nrow(data)){
      if(final_ratings$team_market.x[j] == data$team_market.x[i] & final_ratings$n[j] == data$team_game_number[i]){
        
        final_ratings$final_rating[j] = data$rating[i]
        
      }
    }
  }
  
  final_ratings = final_ratings %>%
    arrange(desc(final_rating))
  
  return(final_ratings)
  
}

find_final_volatility = function(data){
  
  final_ratings = data %>%
    group_by(team_market.x) %>%
    summarise(n=n())
  
  final_ratings$final_volatility = 0
  
  for(j in 1:nrow(final_ratings)){
    for(i in 1:nrow(data)){
      if(final_ratings$team_market.x[j] == data$team_market.x[i] & final_ratings$n[j] == data$team_game_number[i]){
        
        final_ratings$final_volatility[j] = data$volatility[i]
        
      }
    }
  }
  
  final_ratings = final_ratings %>%
    arrange(desc(final_volatility))
  
  return(final_ratings)
  
}


tune_hyperparameters = function(data,  gd_scale = c(0.75,1,1.25), last_year_ratings = NULL, last_year_vol = NULL, sd_scale = c(0.5,0.25,0.1),  home_effect = c(0.2,0.3,0.4), vol_scale = c(1.75,2), vol_step = c(0.5,0.75,1), update_scale = c(0.05,0.1,0.2)){
  
  df = data
  
  three_way_results = c()
  brier_scores = c()
  mses = c()
  hypers = c()
  
  
  for(v in 1:length(vol_scale)){
    
    for(s in 1:length(vol_step)){
      
      for(h in 1:length(home_effect)){
        
        for(k in 1:length(gd_scale)){
          
          for(m in 1:length(sd_scale)){
            
            for(b in 1:length(update_scale)){
              
              hyperparameters = c(vol_scale[v], vol_step[s], home_effect[h], gd_scale[k], sd_scale[m], update_scale[b])
              
              df = create_ratings(df,hyperparameters,last_year_ratings, last_year_vol)
              
              performance = get_performance_measures(df)
              
              three_way_results = c(three_way_results,performance[1])
              brier_scores = c(brier_scores,performance[2])
              mses = c(mses, performance[3])
              hypers = c(hypers,paste0(v,s,h,k,m,b))
              
              
              print(v)
              
            }
          }}}}}
  
  results = list(threeway = three_way_results, brier = brier_scores,mses = mses,hyperparameters = hypers)
  
  hyper_index = which.min(results[["mses"]])
  
  tuning_indices = results[["hyperparameters"]][hyper_index]
  
  params_indices = as.integer(strsplit(tuning_indices, split = "")[[1]])
  
  initial_vol = vol_scale[params_indices[1]]
  
  vol_param = vol_step[params_indices[2]]
  
  he_param = home_effect[params_indices[3]]
  
  gd_param = gd_scale[params_indices[4]]
  
  sd_param = sd_scale[params_indices[5]]
  
  update_param = update_scale[params_indices[6]]
  
  hyperparameters = c(initial_vol,vol_param,he_param,gd_param,sd_param,update_param)
  
  return(list(best = hyperparameters,results = results))  
}

get_performance_measures = function(data){
  
  df = data
  df$pred_result = 0
  brier = 0
  
  df = df %>%
    mutate(pred_score_margin = prior_rating - opp_prior_rating)
  
  for(j in 1:nrow(df)){
    
    if(df$win_probability[j] == max(df$win_probability[j],df$draw_probability[j],df$loss_probability[j])){
      
      df$pred_result[j] = 1
    }
    
    if(df$draw_probability[j] == max(df$win_probability[j],df$draw_probability[j],df$loss_probability[j])){
      
      df$pred_result[j] = 0.5
    }
    if(df$result[j] == 1){
      
      brier = brier + (df$win_probability[j] - 1)^2 + (df$draw_probability[j])^2 + (df$loss_probability[j])^2
      
    }
    
    if(df$result[j] == 0.5){
      
      brier = brier + (df$win_probability[j])^2 + (df$draw_probability[j] - 1)^2 + (df$loss_probability[j])^2
      
    }
    
    if(df$result[j] == 0){
      
      brier = brier + (df$win_probability[j])^2 + (df$draw_probability[j])^2 + (df$loss_probability[j] - 1)^2
      
    }
    
  }
  
  three_way_results = mean(df$pred_result == df$result)
  
  brier_score = brier/nrow(df)
  
  mse = sqrt(mean((df$goal_differential - df$pred_score_margin)^2))
  
  return(c(three_way_results,brier_score,mse))
  
}


run_ratings_model = function(data, league){
  
  if(league == "womens"){
    lower = 1
    upper = (length(data)/2) - 1
  }
  else{
    lower = (length(data)/2) + 1
    upper = length(data) - 1
  }
  
  for(i in lower:upper){
    
    if(i == lower){
      
      hyperparameters = tune_hyperparameters(data[[i]])
      
      data[[i]] =  create_ratings(data[[i]], hyperparameters$best)
      
    }
    
    data[[i+1]] = create_ratings(data[[i+1]], hyperparameters$best, find_final_ratings(data[[i]]), find_final_volatility(data[[i]]))
    
  }
  
  return(list(data,hyperparameters$best))
}


predict_score_margin = function(data, alpha){
  
  for(i in 1:length(data)){
    
    df = data[[i]]
    
    df = df %>%
      mutate(pred_score_margin = 
               prior_rating - opp_prior_rating) %>%
      mutate(pred_score_margin_lower = 
               pred_score_margin - qnorm(1 - (alpha/2))*sqrt(prior_volatility^2 + opp_prior_volatility^2),
             pred_score_margin_upper = 
               pred_score_margin + qnorm(1 - (alpha/2))*sqrt(prior_volatility^2 + opp_prior_volatility^2)
      )
    
    
    data[[i]] = df  
    
  }
  
  return(data)
}

calculate_empirical_coverage = function(data,alpha,home_effect,league){
  coverage = list()
  
  if(league == "womens"){
    lower = 1
    upper = (length(data)/2)
  }
  else{
    lower = (length(data)/2) + 1
    upper = length(data)
  }
  
  for(i in upper:lower){
    
    cov = c()
    df = data[[i]]
    
    for(j in 1:length(alpha)){
      
      df = predict_score_margin(df,home_effect,alpha[j])
      value = mean(df$goal_differential <= df$pred_score_margin_upper & df$goal_differential >= df$pred_score_margin_lower)
      cov = c(cov, value)
      
    }
    
    coverage[[length(coverage) + 1]] = cov
  }
  
  return(coverage)
}


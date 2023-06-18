create_baseline_ratings = function(data, hyperparameters, last_year_ratings = NULL){
  
  df = data
  season_year = df$season_year[1]
  initial_rating = hyperparameters[1]
  update_param = hyperparameters[2]
  
  df$rating = initial_rating
  df$prior_rating = initial_rating
  df$opp_prior_rating = initial_rating
  df$win_probability = 0
  df$draw_probability = 0
  df$loss_probability = 0
  df$offensive_rating = 0
  df$defensive_rating = 0
  df$prior_off_rating = 0
  df$prior_def_rating = 0
  df$opp_prior_off_rating = 0
  df$opp_prior_def_rating = 0
  df$update_size = 0
  df$prior_update_size = 0
  
  logistic_function = function(prior_rating, opponent_prior_rating){
    
    ratings_diff = prior_rating - opponent_prior_rating
    
    prob = 1/(1 + 10^(-ratings_diff/600))
    
    return(prob)
  }
  
  
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
      
      prior_update_size = df$update_size[index]
      
      df$prior_update_size[j] = prior_update_size
      
      prior_offensive_rating = df$offensive_rating[index]
      
      df$prior_off_rating[j] = prior_offensive_rating
      
      prior_defensive_rating = df$defensive_rating[index]
      
      df$prior_def_rating[j] = prior_defensive_rating
    }
    
    
    
    if(game_number == 1 & is.null(last_year_ratings)){
      prior_rating = initial_rating
      prior_offensive_rating = 1
      prior_defensive_rating = 1
      prior_update_size = 0}
    
    if(game_number == 1 & !is.null(last_year_ratings)){
      index = which(last_year_ratings$team_market.x == team)
      if(length(index)==0){
        prior_rating = initial_rating
      }
      else{
        prior_rating = last_year_ratings$final_rating[index]
        df$prior_rating[j] = prior_rating}
      prior_offensive_rating = 1
      prior_defensive_rating = 1
      prior_update_size = 0}
    
    if(opponent_game_number > 1){
      
      opponent_indices = which(df$team_market.x == df$team_market[j])
      
      opponent_index = opponent_indices[as.integer(opponent_game_number-1)]
      
      opponent_prior_rating = df$rating[opponent_index]
      
      df$opp_prior_rating[j] = opponent_prior_rating
      
      opponent_prior_offensive_rating = df$offensive_rating[opponent_index]
      
      opponent_prior_defensive_rating = df$defensive_rating[opponent_index]
      
      df$opp_prior_off_rating[j] = opponent_prior_offensive_rating
      
      df$opp_prior_def_rating[j] = opponent_prior_defensive_rating
      
    }
    
    if(opponent_game_number == 1 & season_year == 2020){
      opponent_prior_rating = initial_rating
      opponent_prior_offensive_rating = 1
      opponent_prior_defensive_rating = 1
    }
    
    if(opponent_game_number == 1 & season_year != 2020){
      opponent_index = which(last_year_ratings$team_market.x == opponent)
      if(length(opponent_index)==0){
        opponent_prior_rating = initial_rating
      }
      else{
        opponent_prior_rating = last_year_ratings$final_rating[opponent_index]
        df$opp_prior_rating[j] = opponent_prior_rating
      }
      opponent_prior_offensive_rating = 1
      opponent_prior_defensive_rating = 1}
    
    result = df$result[j]
    
    win_probability = logistic_function(prior_rating,opponent_prior_rating)
    
    df$win_probability[j] = win_probability
    df$loss_probability[j] = 1 - win_probability
    
    update_size = update_param*(result - win_probability) 
    
    df$rating[j] = prior_rating +
      update_size
    
    df$update_size[j] = update_size
    
    goals_scored = df$goals[j]
    
    goals_allowed = df$opp_goals[j]
    
    offensive_update = (sqrt(prior_offensive_rating* opponent_prior_defensive_rating) + goals_scored)/2
    
    df$offensive_rating[j] = offensive_update
    
    defensive_update= (sqrt(prior_defensive_rating*opponent_prior_offensive_rating) + goals_allowed)/2
    
    df$defensive_rating[j] = defensive_update
    
  }
  return(df) 
  
}

tune_baseline_hyperparameters = function(data,last_year_ratings = NULL, update_scale = c(15,25,35,45), initial_rating_scale = c(1400,1500,1600)){
  
  df = data
  
  three_way_results = c()
  two_way_results = c()
  brier_scores = c()
  mses = c()
  hypers = c()
  
  for(l in 1:length(initial_rating_scale)){
    
    for(u in 1:length(update_scale)){
      
      hyperparameters = c(initial_rating_scale[l], update_scale[u])
      
      if(is.null(last_year_ratings)){
        
        df = create_baseline_ratings(df,hyperparameters)}
      
      else{
        
        df = create_baseline_ratings(df,hyperparameters,last_year_ratings)
      }
      
      performance = get_performance_measures(df)
      
      three_way_results = c(three_way_results,performance[1])
      brier_scores = c(brier_scores,performance[2])
      mses = c(mses, performance[3])
      hypers = c(hypers,paste0(l,u))
      
      
      print(l)
      
    }
  }
  
  results = list(threeway = three_way_results, brier = brier_scores,mses = mses,hyperparameters = hypers)
  
  hyper_index = which.min(results[["mses"]])
  
  tuning_indices = results[["hyperparameters"]][hyper_index]
  
  params_indices = as.integer(strsplit(tuning_indices, split = "")[[1]])
  
  initial_rating_param = initial_rating_scale[params_indices[1]]
  
  update_param = update_scale[params_indices[2]]
  
  hyperparameters = c(initial_rating_param,update_param)
  
  return(list(best = hyperparameters,results = results))  
}


run_baseline_model = function(data, league){
  
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
      
      hyperparameters = tune_baseline_hyperparameters(data[[i]])
      
      data[[i]] =  create_baseline_ratings(data[[i]], hyperparameters$best)
      
    }
    
    data[[i+1]] = create_baseline_ratings(data[[i+1]], hyperparameters$best, find_final_ratings(data[[i]]))
    
    
  }
  
  return(data) 
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


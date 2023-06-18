create_offensive_and_defensive_stats = function(data){
  
  for(i in 1:length(data)){ 
    
    df = data[[i]] 
    
    df = df %>%
      group_by(team_market.x) %>%
      mutate(goal_avg = lag(cumsum(goals)/team_game_number,1),
             shots_avg = lag(cumsum(shots)/team_game_number,1),
             corners_avg = lag(cumsum(corners)/team_game_number,1),
             offsides_avg = lag(cumsum(offsides)/team_game_number,1),
             shots_on_target_pct_avg = lag(cumsum(shots_on_target_percentage)/team_game_number,1),
             shots_saved_avg = lag(cumsum(shots_saved)/team_game_number,1),
             save_pct_avg = lag(cumsum(save_percentage)/team_game_number,1),
             goal_kicks_avg = lag(cumsum(goal_kicks)/team_game_number,1),
             prior_goals_1 = lag(goals,1),
             prior_goals_2 = lag(goals,2),
             prior_shots_1 = lag(shots,1),
             prior_shots_2 = lag(shots,2),
             prior_corners_1 = lag(corners,1),
             prior_corners_2 = lag(corners,2),
             prior_offsides_1 = lag(offsides,1),
             prior_offsides_2 = lag(offsides,2),
             prior_shots_on_target_pct_1 = lag(shots_on_target_percentage,1),
             prior_shots_on_target_pct_2 = lag(shots_on_target_percentage,2),
             prior_shots_saved_1 = lag(shots_saved,1),
             prior_shots_saved_2 = lag(shots_saved,2),
             prior_save_percentage_1 = lag(save_percentage,1),
             prior_save_percentage_2 = lag(save_percentage,2),
             prior_goal_kicks_1 = lag(goal_kicks,1),
             prior_goal_kicks_2 = lag(goal_kicks,2)
      ) %>%
      ungroup()
    
    df = df %>%
      group_by(team_market) %>%
      mutate(opp_goal_avg = 
               lag(cumsum(opp_goals)/opponent_team_game_number,1),
             opp_shots_avg = lag(cumsum(opp_shots)/opponent_team_game_number,1),
             opp_corners_avg = lag(cumsum(opp_corners)/opponent_team_game_number,1),
             opp_offsides_avg = lag(cumsum(opp_offsides)/opponent_team_game_number,1),
             opp_shots_on_target_pct_avg = lag(cumsum(opp_shots_on_target_percentage)/opponent_team_game_number,1),
             opp_shots_saved_avg = lag(cumsum(opp_shots_saved)/opponent_team_game_number,1),
             opp_save_pct_avg = lag(cumsum(opp_save_percentage)/opponent_team_game_number,1),
             opp_goal_kicks_avg = lag(cumsum(opp_goal_kicks)/opponent_team_game_number,1),
             rating_avg_opp = cumsum(opp_prior_rating)/opponent_team_game_number,
             opp_rating_avg_opp = cumsum(prior_rating)/opponent_team_game_number,
             opp_prior_goals_1 = lag(opp_goals,1),
             opp_prior_goals_2 = lag(opp_goals,2),
             opp_prior_shots_1 = lag(opp_shots,1),
             opp_prior_shots_2 = lag(opp_shots,2),
             opp_prior_corners_1 = lag(opp_corners,1),
             opp_prior_corners_2 = lag(opp_corners,2),
             opp_prior_offsides_1 = lag(opp_offsides,1),
             opp_prior_offsides_2 = lag(opp_offsides,2),
             opp_prior_shots_on_target_pct_1 = lag(opp_shots_on_target_percentage,1),
             opp_prior_shots_on_target_pct_2 = lag(opp_shots_on_target_percentage,2),
             opp_prior_shots_saved_1 = lag(opp_shots_saved,1),
             opp_prior_shots_saved_2 = lag(opp_shots_saved,2),
             opp_prior_save_percentage_1 = lag(opp_save_percentage,1),
             opp_prior_save_percentage_2 = lag(opp_save_percentage,2),
             opp_prior_goal_kicks_1 = lag(opp_goal_kicks,1),
             opp_prior_goal_kicks_2 = lag(opp_goal_kicks,2)
      ) %>%
      ungroup()
    data[[i]] = df
  }
  return(data)
}


create_historical_stats = function(data){
  
  hist = list()  
  
  for(j in 1:length(data)){  
    
    df = data[[j]]
    
    df$date = as.Date(df$date)
    
    dates = unique(df$date)
    
    hist_df = data.frame()
    
    for(i in 1:length(dates)){
      
      temp_data = df[df$date < dates[i],]
      
      historical_stats = temp_data %>%
        summarise_at(.vars = opponent_variables, mean, na.rm = TRUE)
      
      historical_stats$date = dates[i]
      
      hist_df = rbind(hist_df,historical_stats)
    }
    hist_df = na.omit(hist_df)
    hist[[j]] = hist_df
  } 
  return(hist)
}

apply_exponential_smoothing = function(data,alpha){
  
  split_dfs = split(data,data$team_market.x)
  
  for(i in 1:length(split_dfs)){
    
    df = split_dfs[[i]]
    
    df$exp_smooth_off_rating = df$new_offensive_rating
    df$exp_smooth_def_rating = df$new_defensive_rating
    
    if(nrow(df) > 1){
      for(j in 2:nrow(df)){
        
        if(df$new_offensive_rating[j-1] != 0){
          
          df$exp_smooth_off_rating[j] =  (1-alpha)*df$exp_smooth_off_rating[j-1] + alpha*df$new_offensive_rating[j]}
        
        if(df$new_defensive_rating[j-1] != 0){
          
          df$exp_smooth_def_rating[j] =  (1-alpha)*df$exp_smooth_def_rating[j-1] + alpha*df$new_defensive_rating[j]}
        
      }}
    
    split_dfs[[i]] = df
  }
  
  data = bind_rows(split_dfs)%>%
    arrange(date)
  
  return(data)
}

create_offensive_defensive_ratings = function(prior_year_data, data, type, historical_data){ 
  
  df = data %>%
    dplyr::select(-all_of(opponent_variables))
  
  df = left_join(df, historical_data, by = "date") %>%
    arrange(date)
  
  model_full = df[complete.cases(df), ]
  model_nas = df[!complete.cases(df), ]
  model_df = na.omit(rbind(prior_year_data,
                           model_full)) 
  current_year = data$season_year[1]
  prop = 1 - mean(model_df$season_year == current_year)
  
  split = initial_time_split(model_df[,c("location_type","goals","opp_goals",team_variables, opponent_variables)], prop = prop)
  train = training(split)
  test = testing(split)
  folds =  vfold_cv(train)
  
  rf_model = rand_forest(mtry = tune(), min_n = tune(), trees = 250) %>% 
    set_engine("ranger", importance = "impurity", num.threads = 4) %>% 
    set_mode("regression")
  
  if(type == "offensive"){
    rf_workflow = workflow() %>%
      add_model(rf_model) %>%
      add_formula(goals ~ .)}
  
  else{
    rf_workflow = workflow() %>%
      add_model(rf_model) %>%
      add_formula(opp_goals ~ .)}
  
  rf_res = rf_workflow %>% 
    tune_grid(
      resamples = folds,
      grid = 25,
      control = control_grid(save_pred = TRUE),
      metrics = metric_set(rmse)
    )
  
  rf_final = rf_res %>%
    select_best("rmse")
  
  final_wf = rf_workflow %>%
    finalize_workflow(rf_final)
  
  final_fit = final_wf %>%
    last_fit(split)
  
  preds = final_fit %>%
    collect_predictions()
  
  if(type == "offensive"){
    model_full$new_offensive_rating = preds$.pred
    model_nas$new_offensive_rating = 0
  }
  
  else{
    
    model_full$new_defensive_rating = preds$.pred
    model_nas$new_defensive_rating = 0
  }
  
  ret = rbind(model_full,model_nas) %>%
    arrange(date)
  if(type == "offensive"){
    
    return(ret$new_offensive_rating)}
  
  else{
    
    return(ret$new_defensive_rating)}
}

run_offensive_defensive_model = function(data, league, alpha){
  
  historical_stats = create_historical_stats(data)
  
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
      
      train$new_offensive_rating = 0
      train$new_defensive_rating = 0
      train$exp_smooth_off_rating = 0
      train$exp_smooth_def_rating = 0
    }
    
    train = rbind(train, data[[i]])
    
    test = data[[i+1]]
    
    if(i == lower + 1){
      
      test$new_offensive_rating = 0
      test$new_defensive_rating = 0
      test$exp_smooth_off_rating = 0
      test$exp_smooth_def_rating = 0
    }
    
    off_rating = create_offensive_defensive_ratings(
      train,test,"offensive",historical_stats[[i+1]])
    
    def_rating = create_offensive_defensive_ratings(
      train,test,"defensive",historical_stats[[i+1]])
    
    data[[i+1]]$new_offensive_rating = off_rating
    data[[i+1]]$new_defensive_rating = def_rating
    
    data[[i+1]] = apply_exponential_smoothing(data[[i+1]],alpha)
    
  }
  
  return(data)
}

get_prior_off_def_ratings = function(data){
  
  df = data
  df$prior_new_off_rating = 0
  df$prior_new_def_rating = 0
  df$prior_exp_smooth_off_rating = 0
  df$prior_exp_smooth_def_rating = 0
  
  for(j in 1:nrow(df)){
    
    team = df$team_market.x[j]
    
    game_number = df$team_game_number[j]
    
    if(game_number > 1){
      
      indices = which(df$team_market.x == df$team_market.x[j])
      
      index = indices[as.integer(game_number-1)]
      
      df$prior_new_off_rating[j] = df$new_offensive_rating[index]
      
      df$prior_new_def_rating[j] = df$new_defensive_rating[index]
      
      df$prior_exp_smooth_off_rating[j] = df$exp_smooth_off_rating[index]
      
      df$prior_exp_smooth_def_rating[j] = df$exp_smooth_def_rating[index]
      
    }
  }
  return(df) 
}

find_final_off_ratings = function(data){
  
  final_ratings = data %>%
    group_by(team_market.x) %>%
    summarise(n=n())
  
  final_ratings$final_off_rating = 0
  
  for(j in 1:nrow(final_ratings)){
    for(i in 1:nrow(data)){
      if(final_ratings$team_market.x[j] == data$team_market.x[i] & final_ratings$n[j] == data$team_game_number[i]){
        
        final_ratings$final_off_rating[j] = data$exp_smooth_off_rating[i]
        
      }
    }
  }
  
  final_ratings = final_ratings %>%
    arrange(desc(final_off_rating))
  
  return(final_ratings)
  
}

find_final_def_ratings = function(data){
  
  final_ratings = data %>%
    group_by(team_market.x) %>%
    summarise(n=n()) %>%
    filter(n>1)
  
  final_ratings$final_def_rating = 0
  
  for(j in 1:nrow(final_ratings)){
    for(i in 1:nrow(data)){
      if(final_ratings$team_market.x[j] == data$team_market.x[i] & final_ratings$n[j] == data$team_game_number[i]){
        
        final_ratings$final_def_rating[j] = data$exp_smooth_def_rating[i]
        
      }
    }
  }
  
  final_ratings = final_ratings %>%
    arrange(final_def_rating)
  
  return(final_ratings)
  
}




join_data = function(team_game_stat, team){
  
  joined_data = left_join(team_game_stat, team, by = "team_id")
  
  team$opponent_team_id = team$team_id
  
  joined_data = left_join(joined_data, team, by =   "opponent_team_id")
  
  womens = joined_data %>% 
    filter(league_name.x == "NCAA WOMEN SOCCER",   division_name.x == "D-I", division_name.y == "D-I")
  
  mens = joined_data %>%
    filter(league_name.x == "NCAA MEN SOCCER",
           division_name.x == "D-I",division_name.y == "D-I")
  
  return(list(womens = womens,mens = mens))
}

preprocess_data = function(data){
  
  for(j in 1:length(data)){
    
    df = data[[j]]
    
    variables = c("season_year", "date", "team_game_number","opponent_team_game_number", "game_id", "team_id.x","team_name.x", "team_market.x", "team_conference_id", "team_id.y", "team_name", "team_market", "location_type", "goal_differential","goals","opp_goals","corners","fouls_committed", "fouls_won", "free_kicks", "goal_kicks", "offsides", "red_cards", "yellow_cards", "shots", "shots_on_target", "shots_off_target", "shots_on_target_percentage", "shots_on_wood_work", "shots_saved", "save_percentage", "throw_ins", "opp_corners",   "opp_fouls_committed", "opp_fouls_won", "opp_free_kicks", "opp_goal_kicks", "opp_offsides", "opp_red_cards", "opp_yellow_cards", "opp_shots", "opp_shots_on_target", "opp_shots_off_target", "opp_shots_on_target_percentage", "opp_shots_on_wood_work", "opp_shots_saved", "opp_save_percentage", "opp_throw_ins")
    
    df = df[,variables]
    
    for(i in 1:nrow(df)){
      
      df$goal_differential[i] = df$goals[i] - df$opp_goals[i]
      
      if(df$goals[i] > df$shots[i]){
        
        df$shots[i] = floor(df$goals[i] + mean(df$shots) - mean(df$goals))
      }
      
      if(df$shots_on_target[i] == 0 & df$shots_off_target[i] == 0){
        df$shots_on_target_percentage[i] = NA
      }
      
      if(df$shots[i] != df$shots_on_target[i] + df$shots_off_target[i] & df$shots_on_target[i] != 0){
        df$shots_off_target[i] = 
          df$shots[i] - df$shots_on_target[i]
      }
      
      if(df$opp_goals[i] > df$opp_shots[i]){
        df$opp_shots[i] = floor(df$opp_goals[i] + mean(df$shots) - mean(df$goals))
      }
      
      if(df$opp_shots_on_target[i] == 0 & df$opp_shots_off_target[i] == 0){
        
        df$opp_shots_on_target_percentage[i] = NA
      }
      
      if(df$opp_shots[i] != df$opp_shots_on_target[i] + df$opp_shots_off_target[i] & df$opp_shots_on_target[i] != 0){
        df$opp_shots_off_target[i] = 
          df$opp_shots[i] - df$opp_shots_on_target[i]
      }
    }
    
    on_target_pct = df %>%
      filter(!is.na(shots_on_target_percentage)) %>%
      summarise(mean = mean(shots_on_target_percentage))
    
    for(i in 1:nrow(df)){
      if(is.na(df$shots_on_target_percentage[i])){
        df$shots_on_target_percentage[i] = on_target_pct$mean
      }
      if(is.na(df$opp_shots_on_target_percentage[i])){
        df$opp_shots_on_target_percentage[i] = on_target_pct$mean
      }
    }
    
    for(i in 1:nrow(df)){
      
      if(df$shots_on_target[i] == 0 &
         df$shots_off_target[i] == 0){
        df$shots_on_target[i] = 
          floor(df$shots_on_target_percentage[i]*df$shots[i])
      }
      df$shots_off_target[i] = df$shots[i] - 
        df$shots_on_target[i]
      
      if(df$opp_shots_on_target[i] == 0 &
         df$opp_shots_off_target[i] == 0){
        df$opp_shots_on_target[i] = 
          floor(df$opp_shots_on_target_percentage[i]*df$opp_shots[i])}
      df$opp_shots_off_target[i] = df$opp_shots[i] - df$opp_shots_on_target[i]
      
    }
    
    df = df %>%
      mutate(shots_saved = opp_shots_on_target - opp_goals,
             opp_shots_saved = shots_on_target -
               goals)
    
    for(i in 1:nrow(df)){
      
      if(df$opp_shots_on_target[i] > 0){
        df$save_percentage[i] =
          df$shots_saved[i]/df$opp_shots_on_target[i]
      }
      
      if(df$opp_shots_on_target[i] == 0)
      {
        df$save_percentage[i] = 1
      }
      
      if(df$shots_on_target[i] > 0){
        df$opp_save_percentage[i] =
          df$opp_shots_saved[i]/df$shots_on_target[i]
      }
      if(df$shots_on_target[i] == 0)
      {
        df$opp_save_percentage[i] = 1
      }
      if(df$save_percentage[i] < 0){
        df$save_percentage[i] = 0
      }
      
      if(df$opp_save_percentage[i] < 0){
        df$opp_save_percentage[i] = 0
      }
      
    }
    
    data[[j]] = df
  }
  return(data)
}

filter_data = function(data){
  
  dataframes = list() 
  names = c()
  
  for(i in 1:length(data)){
    
    df = data[[i]]
    years = unique(df$season_year) %>%
      sort()
    league = c("womens_model","mens_model")
    
    for(j in 1:length(years)){
      
      df_temp = df %>%
        filter(season_year == years[j]) %>%
        arrange(date)
      
      dataframes[[length(dataframes) + 1]] = df_temp
      names = append(names, paste0(league[i],"_",years[j]))
      
    }}
  
  names(dataframes) = names
  return(dataframes)
  
}

create_result_and_shot_diff = function(data){
  
  for(j in 1:length(data)){
    
    df = data[[j]]
    
    df = df %>%
      mutate(diff_in_shots = shots - opp_shots)
    
    for(i in 1:nrow(df)){
      
      if(df$goal_differential[i] > 0){
        df$result[i] = 1
      }
      else if(df$goal_differential[i] == 0){
        df$result[i] = 0.5
      }
      else
        df$result[i] = 0
    }
    
    data[[j]] = df
    
  }
  
  return(data) 
  
}

fix_game_numbers = function(data){
  
  for(i in 1:length(data)){
    
    df = data[[i]]
    
    df$team_game_number = as.integer(df$team_game_number)
    
    split_dfs = split(df,df$team_id.x)
    
    for(j in 1:length(split_dfs)){
      df_temp = split_dfs[[j]]
      n = nrow(df_temp)
      df_temp$team_game_number = 1:n
      split_dfs[[j]] = df_temp
    }
    
    df = bind_rows(split_dfs)%>%
      arrange(date)
    
    data[[i]] = df
    
  }
  
  return(data)
  
}

fix_opp_game_numbers = function(data){
  
  for(i in 1:length(data)){
    
    df = data[[i]] 
    
    df$opponent_team_game_number = as.integer(df$opponent_team_game_number)
    
    opponent_game_number = rep(0,nrow(df))  
    
    for(j in 1:length(unique(df$team_id.x))){
      number = 0
      for(k in 1:nrow(df)){
        
        if(df$team_id.y[k] == unique(df$team_id.x)[j]){
          
          number = number + 1
          opponent_game_number[k] = number
        }
      }
    }
    df$opponent_team_game_number = opponent_game_number
    
    data[[i]] = df
    
  }
  return(data) 
}

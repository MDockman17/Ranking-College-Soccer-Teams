find_season_rankings = function(data,dates){
  
  df = data.frame()
  cols_rating = c()
  cols_rank = c()
  
  for(i in 1:length(dates)){
    
    ranks = find_ranks(data, dates[i])
    
    ranks = ranks[,c("team_market.x","rank","rating")]
    
    ranks$week_number = paste0("Poll ",i)
    
    df = rbind(df,ranks)
    
    cols_rank = c(cols_rank, paste0("Poll_Ranking_",i))
    
    cols_rating = c(cols_rating, paste0("Poll_Rating_",i))
    
  }
  
  df = df %>%
    pivot_wider(names_from = week_number, values_from = c(rank,rating))
  
  colnames(df) = c("team_market.x",cols_rank,cols_rating)
  
  return(df)
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

get_performance_measures = function(data){
  
  df = data
  
  df = df %>%
    mutate(pred_result = case_when(
      win_probability >= 0.48 & win_probability <= 0.52 ~ "0.5",
      win_probability > 0.52 ~ "1",
      win_probability < 0.48 ~ "0"
    ),
    pred_score_margin = prior_rating -            opp_prior_rating)
  
  df$pred_result = as.numeric(df$pred_result)
  
  three_way_results = mean(df$pred_result == df$result)
  
  df_filter = df %>%
    filter(result != 0.5) %>%
    mutate(pred_result = ifelse(win_probability>=0.5,1,0))
  
  two_way_results = mean(df_filter$pred_result == df_filter$result)
  
  brier_score = mean((df_filter$win_probability - df_filter$result)^2)
  
  mse = sqrt(mean((df$goal_differential - df$pred_score_margin)^2))
  
  return(c(three_way_results,two_way_results,brier_score,mse))
  
}

find_ranks = function(data,day){
  
  df = data
  
  df$date = as.Date(df$date)
  
  df = df[df$date <= day,]
  
  ratings = df %>%
    group_by(team_market.x, team_conference_id) %>%
    summarise(n=n())
  
  ratings$rating = 0
  
  for(j in 1:nrow(ratings)){
    for(i in 1:nrow(df)){
      if(ratings$team_market.x[j] == df$team_market.x[i] & ratings$n[j] == df$team_game_number[i]){
        
        ratings$rating[j] = df$rating[i]
        
      }
    }
  }
  
  ratings = ratings %>%
    ungroup() %>%
    arrange(desc(rating)) %>%
    mutate(rank = 1:nrow(ratings))
  
  return(ratings)
  
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
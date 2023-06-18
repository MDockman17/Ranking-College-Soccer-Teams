calculate_accuracy_over_time = function(data){
  
  accuracy_over_time = c()
  
  for(i in 1:max(data$team_game_number)){
    df = data %>%
      filter(team_game_number == i)
    
    pred = mean(df$pred_result == df$result)
    
    accuracy_over_time = c(accuracy_over_time,pred)
  }
  return(accuracy_over_time)
}

calculate_mse_over_time = function(data){
  
  mses = c()
  
  for(i in 1:max(data$team_game_number)){
    df = data %>%
      filter(team_game_number == i) %>%
      mutate(pred_score_margin = prior_rating - opp_prior_rating)
    
    value = sqrt(mean((df$goal_differential - df$pred_score_margin)^2))
    
    mses = c(mses, value)
  }
  return(mses)
}

find_ranks = function(data,day){
  
  options(dplyr.summarise.inform = FALSE)
  
  df = data
  
  df$date = as.Date(df$date)
  
  df = df[df$date <= day,]
  
  ratings = df %>%
    group_by(team_market.x, team_conference_id) %>%
    summarise(n=n(),
              wins = sum(result ==1),
              draws = sum(result == 0.5),
              losses = sum(result == 0),
              total_goals = sum(goals))
  
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

find_season_rankings = function(data,dates){
  
  df = data.frame()
  cols_rating = c()
  cols_rank = c()
  
  for(i in 1:length(dates)){
    
    ranks = find_ranks(data, dates[i])
    
    ranks = ranks[,c("team_market.x","rank","rating", "n", "wins", "draws", "losses", "total_goals")]
    
    ranks$week_number = paste0("Poll_",i)
    
    df = rbind(df,ranks)
    
    cols_rank = c(cols_rank, paste0("Poll_Ranking_",i))
    
    cols_rating = c(cols_rating, paste0("Poll_Rating_",i))
    
  }
  
  df = df %>%
    pivot_wider(names_from = week_number, values_from = c(rank,rating, n, wins, losses, draws, total_goals))
  
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

calculate_ranks_deviation = function(data, ranking_dates){
  
  df = data
  team_deviation = c()
  
  for(j in 3:(length(ranking_dates)+1)){
    
    prior_vector = df %>% pull(colnames(df)[j-1])
    
    current_vector = df %>% pull(colnames(df)[j])
    
    value = abs(prior_vector - current_vector)
    
    team_deviation = c(team_deviation, value)
    
  }
  
  return(team_deviation)
}

run_rating_initialization_experiment = function(df, final_ratings, final_volatility, hyperparameters, rankings_dates, season_rankings, league){
  
  mean_deviation = c()
  mean_pct_deviation = c()
  mean_ratings_diff = c()
  
  for(i in 1:10){
    
    fake_final_ratings = final_ratings
    
    fake_final_ratings$final_rating = fake_final_ratings$final_rating + rnorm(nrow(final_ratings), mean = 0, sd = 0.25)
    
    fake_ratings = create_ratings(df, hyperparameters, fake_final_ratings, final_volatility)
    
    final_fake_season_rankings = find_ranks(fake_ratings, rankings_dates[length(rankings_dates)]) %>%
      arrange(team_market.x)
    
    season_rankings = season_rankings %>%
      arrange(team_market.x)
    
    if(league == "womens"){
      
      value1 = mean(abs(final_fake_season_rankings$rank - season_rankings$rank_Poll_13))
      
      mean_deviation = c(mean_deviation, value1)
      
      value2 = exp(mean(abs((log(final_fake_season_rankings$rank)) - log(season_rankings$rank_Poll_13))))
      
      mean_pct_deviation = c(mean_pct_deviation, value2)
      
      value3 = mean(abs(final_fake_season_rankings$rating - season_rankings$rating_Poll_13))
      
      mean_ratings_diff = c(mean_ratings_diff, value3)
      
    }
    
    else{
      
      value1 = mean(abs(final_fake_season_rankings$rank - season_rankings$rank_Poll_12))
      
      mean_deviation = c(mean_deviation, value1)
      
      value2 = exp(mean(abs((log(final_fake_season_rankings$rank)) - log(season_rankings$rank_Poll_12))))
      
      mean_pct_deviation = c(mean_pct_deviation, value2)
      
      value3 = mean(abs(final_fake_season_rankings$rating - season_rankings$rating_Poll_12))
      
      mean_ratings_diff = c(mean_ratings_diff, value3)
      
    }} 
  
  return(list(mean_abs_rank_deviation = mean_deviation, mean_abs_pct_rank_deviation = mean_pct_deviation, mean_abs_rating_diff = mean_ratings_diff))
}
create_dashboard_data = function(data,dates){
  
  season_rankings = find_season_rankings(data,dates) 
  
  data = data %>%
    group_by(team_market.x)%>%
    summarise(number_matches=n(),
              wins = sum(result ==1),
              draws = sum(result == 0.5),
              losses = sum(result == 0),
              total_goals = sum(goals))
  
  data = left_join(season_rankings, data,  by =    "team_market.x")
  
  return(data)
  
}

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
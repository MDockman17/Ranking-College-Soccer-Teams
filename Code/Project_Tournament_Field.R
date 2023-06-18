find_conference_winners = function(rankings){
  
  conference_winners = rankings %>%
    group_by(team_conference_id) %>%
    summarise(best = max(rating))
  
  conference_winners$team = 0
  
  for(i in 1:nrow(conference_winners)){
    for(j in 1:nrow(rankings)){
      
      if(rankings$rating[j] == conference_winners$best[i] & rankings$team_conference_id[j] == conference_winners$team_conference_id[i]){
        
        conference_winners$team[i] = rankings$team_market.x[j]
      }
    } 
  }
  
  return(conference_winners$team)
}

find_at_large_bids = function(rankings,conference_winners,league){
  
  drop_indices = which(rankings$team_market.x %in% conference_winners)
  
  at_large = rankings[-drop_indices, ]
  
  if(league == "mens"){
    n = 48 - length(conference_winners)}
  
  if(league == "womens"){
    n = 64 - length(conference_winners)}
  
  at_large_bids = at_large$team_market.x[1:n]
  
  return(at_large_bids)
}

project_tournament_field = function(data, league, day){
  
  rankings = find_ranks(data,day)
  
  conference_winners = find_conference_winners(rankings)
  
  at_large_bids = find_at_large_bids(rankings,conference_winners,league)
  
  pred_field = c(conference_winners,at_large_bids)
  
  return(pred_field)
}


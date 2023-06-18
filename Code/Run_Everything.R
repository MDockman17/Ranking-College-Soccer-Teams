run_everything = function(){
  
  boost_data = bring_in_data()
  
  team_game_stat = boost_data[["team_game_stat"]]
  team = boost_data[["team"]]
  
  joined_data = join_data(team_game_stat,team)
  
  processed_data = preprocess_data(joined_data)
  processed_data[["womens"]] = processed_data$womens %>%
    filter(game_id != 6781)
  
  filtered_data = filter_data(processed_data)
  
  data = create_result_and_shot_diff(filtered_data)
  
  data = fix_game_numbers(data)
  
  data = fix_opp_game_numbers(data)
  
  baseline_data = list()
  baseline_womens = run_baseline_model(data,"womens")
  baseline_mens = run_baseline_model(data,"mens")
  baseline_data[1:3] = baseline_womens[1:3]
  baseline_data[4:6] = baseline_mens[4:6]
  
  adjusted_womens_gds = run_goal_diff_model(data,   0.4, "womens")
  data[1:3] = adjusted_womens_gds[1:3]
  adjusted_mens_gds =  run_goal_diff_model(data,0.4,"mens")
  data[4:6] = adjusted_mens_gds[4:6]
  
  data = fix_differing_gds(data)
  
  data_womens = run_ratings_model(data,"womens")
  data_mens = run_ratings_model(data,"mens")
  
  hyperparameters_womens = data_womens[[2]]
  hyperparameters_mens = data_mens[[2]]
  
  data_womens = data_womens[[1]]
  data_mens = data_mens[[1]]
  
  data[1:3] = data_womens[1:3]
  data[4:6] = data_mens[4:6]
  
  data = create_offensive_and_defensive_stats(data)
  
  opponent_variables = c("opp_prior_rating", "opp_prior_off_rating", "opp_prior_def_rating", "opp_goal_avg", "opp_shots_avg","opp_corners_avg", "opp_offsides_avg", "opp_shots_on_target_pct_avg",
                         "opp_shots_saved_avg", "opp_save_pct_avg", "opp_goal_kicks_avg", "rating_avg_opp", "opp_rating_avg_opp", "opp_prior_goals_1", "opp_prior_goals_2", "opp_prior_shots_1" , "opp_prior_shots_2" , "opp_prior_corners_1", 
                         "opp_prior_corners_2", "opp_prior_offsides_1",
                         "opp_prior_offsides_2", "opp_prior_shots_on_target_pct_1",
                         "opp_prior_shots_on_target_pct_2", "opp_prior_shots_saved_1", "opp_prior_shots_saved_2", 
                         "opp_prior_save_percentage_1",
                         "opp_prior_save_percentage_2",
                         "opp_prior_goal_kicks_1",
                         "opp_prior_goal_kicks_2")
  
  team_variables = c("prior_rating", "prior_off_rating", "prior_def_rating", "goal_avg",
                     "shots_avg", "corners_avg", "offsides_avg", 
                     "shots_on_target_pct_avg", "shots_saved_avg", 
                     "save_pct_avg", "goal_kicks_avg", "prior_goals_1", 
                     "prior_goals_2", "prior_shots_1", "prior_shots_2", 
                     "prior_corners_1", "prior_corners_2", "prior_offsides_1", "prior_offsides_2",  
                     "prior_shots_on_target_pct_1",  "prior_shots_on_target_pct_2","prior_shots_saved_1","prior_shots_saved_2","prior_save_percentage_1",  
                     "prior_save_percentage_2", "prior_goal_kicks_1",
                     "prior_goal_kicks_2")
  
  womens_off_def = run_offensive_defensive_model(data,"womens",0.2) 
  mens_off_def = run_offensive_defensive_model(data,"mens",0.2) 
  
  data[1:3] = womens_off_def[1:3]
  data[4:6] = mens_off_def[4:6]
  
  return(list(finalized_model = data, baseline_model = baseline_data, hyperparameters_womens = hyperparameters_womens, hyperparameters_mens = hyperparameters_mens))
}
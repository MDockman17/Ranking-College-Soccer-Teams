rankings = read_csv("womens_2021_final_ratings.csv", show_col_types = FALSE)
rankings_mens = read_csv("mens_2021_final_ratings.csv", show_col_types = FALSE)
comparison = read_csv("rankings_comparison_womens_2021.csv", show_col_types = FALSE)
comparison_mens = read_csv("rankings_comparison_mens_2021.csv", show_col_types = FALSE)



colnames(comparison) = c("Rank", "True Rankings", "Predicted Rankings")

predicted = c(1,3,6,13,7,4,8,2,5,22,10,15,16,9,12,30,14,55,35,25,60,11,28,43,38)

plot_df_comparison = data.frame(
  Team = rep(comparison$`True Rankings`,2),
  Rankings = c(1:25,predicted)
)

plot_df_comparison$true = c(rep("Coaches' Poll",25),rep("Our Model",25))
plot_df_comparison$true = as.factor(plot_df_comparison$true)
plot_df_comparison$Team = factor(plot_df_comparison$Team)
plot_df_comparison$reorder = rep(1:25,2)



ggplot(data=plot_df_comparison, aes(x=reorder(Team,reorder), y = Rankings, col = true, group = true))+
  geom_point(size = 3, alpha = 0.4)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(legend.title=element_blank())+
  ggtitle("")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Team") +
  ylab("Ranking")+
  scale_y_continuous(breaks = seq(0,60,by=5))+
  scale_colour_manual(values = c("orange", "purple"))


colnames(comparison_mens) = c("Rank", "True Rankings", "Predicted Rankings")

predicted_mens = c(7,2,1,29,10,4,9,14,3,16,6,21,8,18,13,22,17,20,36,35,19,5,40,24,76)

plot_df_comparison = data.frame(
  Team = rep(comparison_mens$`True Rankings`,2),
  Rankings = c(1:25,predicted_mens)
)

plot_df_comparison$true = c(rep("Coaches' Poll",25),rep("Our Model",25))
plot_df_comparison$true = as.factor(plot_df_comparison$true)
plot_df_comparison$Team = factor(plot_df_comparison$Team)
plot_df_comparison$reorder = rep(1:25,2)


ggplot(data=plot_df_comparison, aes(x=reorder(Team,reorder), y = Rankings, col = true, group = true))+
  geom_point(size = 3, alpha = 0.4)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(legend.title=element_blank())+
  ggtitle("")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Team") +
  ylab("Ranking")+
  scale_y_continuous(breaks = seq(0,75,by=5))+
  scale_colour_manual(values = c("orange", "purple"))


womens_model_2020 = read.csv("womens_model_2020.csv")
womens_model_2021 = 
  read.csv("womens_model_2021.csv")
womens_model_2022 = 
  read.csv("womens_model_2022.csv")
mens_model_2020 = read.csv("mens_model_2020.csv")
mens_model_2021 = 
  read.csv("mens_model_2021.csv")
mens_model_2022 = 
  read.csv("mens_model_2022.csv")


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

train = womens_model_2020
test = womens_model_2021

set.seed(17)

threshold = 0.4

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

p5 = final_wf %>% 
  extract_fit_parsnip() %>% 
  vip(num_features = length(variables), aesthetics = list(color = "black", fill = "red"))

p5$data$Variable = c("Save Percentage", "Opp Save Percentage", "Shots", "Opp Shots", "Opp Shots Saved", "Shots Saved", "Opp Shots on Target Pct", "Shots on Target Pct", "Opp Goal Kicks", "Goal Kicks", "Offsides", "Opp Offsides")
p5+
  theme_classic()+
  ggtitle("") +
  theme(plot.title = element_text(hjust = 0.5))




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
womens_2020_mses = calculate_mse_over_time(womens_model_2020)
womens_2021_mses = calculate_mse_over_time(womens_model_2021)
womens_2022_mses = calculate_mse_over_time(womens_model_2022)

mens_2020_mses = calculate_mse_over_time(mens_model_2020)
mens_2021_mses = calculate_mse_over_time(mens_model_2021)
mens_2022_mses = calculate_mse_over_time(mens_model_2022)


over_time_df = data.frame(
  
  game_number = c(1:18,
                  1:20,
                  1:15,
                  1:18,
                  1:20,
                  1:13),
  
  mse = c(womens_2020_mses[1:18],womens_2021_mses[1:20],womens_2022_mses[1:15], mens_2020_mses[1:18],
          mens_2021_mses[1:20],mens_2022_mses[1:13]),
  
  Season = c(rep("2020",18),rep("2021",20),rep("2022",15), rep("2020",18),rep("2021",20),rep("2022",13)),
  
  League = c(rep("Womens", 53),rep("Mens",51))
)

over_time_df$game_number = as.integer(over_time_df$game_number)
options(warn=-1)
over_time_df %>%
  filter(Season != 2020, game_number <= 15) %>%
  ggplot(aes(x = game_number, y = mse, group = League, col = League))+
  geom_jitter(alpha=0.4)+
  xlab("Game Number")+
  ylab("RMSE")+
  theme_classic()+
  ggtitle("")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_colour_manual(values = c("blue", "hot pink"))+
  facet_grid(~Season, space = "free")+
  geom_smooth(method = "lm", se = FALSE)+
  ylim(c(1.5,2.75))




season_rankings_womens_2021 = read.csv("season_rankings_womens_2021.csv")

season_rankings_mens_2021 = read.csv("season_rankings_mens_2021.csv")

season_rankings_womens_2021 %>%
  pivot_longer(Poll_Ranking_1:Poll_Ranking_13, names_to = "Poll", values_to = "Rank") %>%
  filter(team_market.x %in% c("Duke")) %>%
  ggplot(aes(x = factor(1:13),y=factor(Rank),group=team_market.x))+
  geom_path(col = "blue")+
  geom_point(col = "blue") +
  theme(legend.title=element_blank())+
  theme_classic()+
  xlab("Week Number")+
  ylab("Rank")+
  labs(color="Team") +
  ggtitle("Duke Women's Ranking Fluctuations Over 2021 Season")+
  geom_segment(aes(x = 4.6, y = 5, xend = 4.6, yend = 2.25), arrow = arrow(length = unit(0.2, "cm")), col = "black")+
  annotate("text", x = 4.6, y = 5.5, label= "Win over UNC", size = 4)+
  geom_segment(aes(x = 7.75, y = 2, xend = 7.5, yend = 4), arrow = arrow(length = unit(0.2, "cm")), col = "black")+
  annotate("text", x = 8.5, y = 1.3, label= "Upset Loss To \n NC State", size = 4)+
  geom_segment(aes(x = 10.5, y = 6, xend = 11.35, yend = 5), arrow = arrow(length = unit(0.2, "cm")), col = "black")+
  annotate("text", x = 10, y = 6.75, label = "Loss to \n Wake Forest", size = 4)+
  theme(plot.title = element_text(hjust = 0.5))




update_ratings_plot = update_ratings = function(result,ratings_diff,sd = 2.5){
  
  if(result == 1){
    
    gradient = (1/sd)*(dnorm(ratings_diff - 1,sd = sd)/pnorm(ratings_diff - 1,sd = sd))}
  
  if(result == 0){
    
    gradient = -(1/sd)*(dnorm(-ratings_diff - 1,sd = sd)/(pnorm(-ratings_diff - 1,sd = sd)))}
  
  if(result == 0.5){
    
    gradient = (-(1/sd)*dnorm(1 - ratings_diff,sd = sd) + (1/sd)*dnorm(-1 - ratings_diff,sd = sd))/((pnorm(1 - ratings_diff,sd = sd) - pnorm(-1 - ratings_diff,sd = sd)))
    
  }
  
  return(gradient) 
  
}



x = seq(-8,8,length.out=1000)
plot_df_grad = data.frame(
  "Rating_Diff" = rep(x,3),
  "Gradient" = c(update_ratings_plot(1,x),update_ratings_plot(0.5,x),update_ratings_plot(0,x)),
  "Result" = c(rep("Win",length(x)),rep("Draw",length(x)),rep("Loss",length(x)))
)

p1 = ggplot(data = plot_df_grad, aes(x = Rating_Diff, y = Gradient, group = Result, col = Result))+
  geom_line() +
  theme_classic()+
  xlab("Rating of Team - Rating of Opponent")+
  ylab("Gradient")+
  labs(color="Result") +
  ggtitle("")+
  geom_segment(aes(x = -3, y = 0.45, xend = -4.75, yend = 0.45), arrow = arrow(length = unit(0.2, "cm")), col = "black")+
  annotate("text", x = -1, y = 0.45, label= "Upset Win", size = 3)+
  geom_segment(aes(x = 3, y = -0.45, xend = 4.75, yend = -0.45), arrow = arrow(length = unit(0.2, "cm")), col = "black")+
  annotate("text", x = 1, y = -0.45, label= "Upset Loss", size = 3)+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_colour_manual(values = c("purple", "red", "blue"))



womens_model_2021$pred_result = factor(womens_model_2021$pred_result)

womens_model_2021$result = factor(womens_model_2021$result)
table(womens_model_2021$result, womens_model_2021$pred_result)

womens_model_2021 = womens_model_2021 %>%
  mutate(character_result = case_when(
    result == 0.5 ~ "Draw",
    result == 1 ~ "Win",
    result == 0 ~ "Loss"
  ),
  character_pred_result = case_when(
    pred_result == 0.5 ~ "Draw",
    pred_result == 1 ~ "Win",
    pred_result == 0 ~ "Loss"
  ))

kable(table(womens_model_2021$character_result, womens_model_2021$character_pred_result))



update_ratings_plot = function(result,rating_diff, sd){
  
  gradient = (result*dnorm(rating_diff,sd=sd)/pnorm(rating_diff,sd=sd)) + ((1-result)*-dnorm(rating_diff, sd=sd)/(1-pnorm(rating_diff,sd=sd)))
  
  return(gradient) 
  
}
y = seq(2.5,4,length.out = 1000)
plot_df_grad = data.frame(
  "Total_Volatility" = rep(y,3),
  "Gradient" = c(update_ratings_plot(1,0,y),update_ratings_plot(0.5,0,y),update_ratings_plot(0,0,y)),
  "Result" = c(rep("Win",length(y)),rep("Draw",length(y)),rep("Loss",length(y)))
)

p2 = ggplot(data = plot_df_grad, aes(x = Total_Volatility, y = Gradient, group = Result, col = Result))+
  geom_line() +
  theme_classic()+
  xlab("Total Volatility")+
  ylab("Gradient")+
  labs(color="Result") +
  annotate("text", x = 3.25, y = 0.4, label = "ratings diff = 0", size = 3)+
  ggtitle("Effect of Volatility on Rating Gradient") +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_colour_manual(values = c("blue", "red", "purple"))




update_volatility_plot_grad = function(result,ratings_diff,prior_volatility,sd){
  
  gradient = (result*-dnorm(ratings_diff,sd = sd)/pnorm(ratings_diff,sd = sd))*(ratings_diff*prior_volatility/(sd^3)) + ((1-result)*dnorm(ratings_diff,sd = sd)/(1-pnorm(ratings_diff,sd = sd)))*(ratings_diff*prior_volatility/(sd^3))
  
  return(gradient) 
  
}

update_volatility_plot_vol = function(result,ratings_diff,prior_volatility,opponent_prior_volatility){
  
  sd = sqrt(prior_volatility^2 + opponent_prior_volatility^2)
  
  gradient = (result*-dnorm(ratings_diff,sd = sd)/pnorm(ratings_diff,sd = sd))*(ratings_diff*prior_volatility/(sd^3)) + ((1-result)*dnorm(ratings_diff,sd = sd)/(1-pnorm(ratings_diff,sd = sd)))*(ratings_diff*prior_volatility/(sd^3))
  
  return(gradient) 
  
}

x = seq(-8,8,length.out=1000)
plot_df_grad = data.frame(
  "Rating_Diff" = rep(x,3),
  "Gradient" = c(update_volatility_plot_grad(1,x,1.75,2.5),update_volatility_plot_grad(0.5,x,1.75,2.5),update_volatility_plot_grad(0,x,1.75,2.5)),
  "Result" = c(rep("Win",length(x)),rep("Draw",length(x)),rep("Loss",length(x)))
)

p3 = ggplot(data = plot_df_grad, aes(x = Rating_Diff, y = Gradient, group = Result, col = Result))+
  geom_line() +
  theme_classic()+
  xlab("Rating of Team - Rating of Opponent")+
  ylab("Gradient")+
  labs(color="Result") +
  ggtitle("Effect of Ratings Difference \n on Volatility Gradient") +
  annotate("text", x = 0, y = 1.1, label = "prior volatility = 1.75\n total volatility = 2.5", size = 2)+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_colour_manual(values = c("blue", "red", "purple"))



y = seq(1.5,2.5,length.out = 1000)
plot_df_grad = data.frame(
  "Total_Volatility" = rep(y,3),
  "Gradient" = c(update_volatility_plot_vol(1,0.5,y,1.75),update_volatility_plot_vol(0.5,0.5,y,1.75),update_volatility_plot_vol(0,0.5,y,1.75)),
  "Result" = c(rep("Win",length(y)),rep("Draw",length(y)),rep("Loss",length(y)))
)

p4 = ggplot(data = plot_df_grad, aes(x = Total_Volatility, y = Gradient, group = Result, col = Result))+
  geom_line() +
  theme_classic()+
  xlab("Prior Volatility")+
  ylab("Gradient")+
  labs(color="Result") +
  ggtitle("Effect of Prior Volatility \n on Volatility Gradient")+
  annotate("text", x = 2, y = 0.02, label = "opponent prior volatility = 1.75\n prior rating difference = 0.5", size = 2)+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_colour_manual(values = c("blue", "red", "purple"))



(p1 + p2)/(p3 + p4)


normal_df = data.frame(x = seq(-8,8,length.out = 1000))
m = 1.5
std = 1.65
ggplot(normal_df, aes(x = x)) + 
  stat_function(fun = dnorm, args = list(mean = m, sd = std))+
  stat_function(fun = dnorm, args = list(mean = m, sd = std), geom = "area", fill = "blue", alpha = 0.2)+
  geom_vline(aes(xintercept = m), col = "red")+
  scale_x_continuous(breaks = c(-5,0,m,5), labels = c(-5,0,expression(mu[A]),5))+
  xlab(expression(paste("Distribution of ",r[A])))+
  ylab("Density")+
  ggtitle("") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme_classic()


m1 = 1.5
std1 = 1.65

m2 = -1
std2 = 1.9
ggplot(normal_df, aes(x = x))  + 
  stat_function(fun = dnorm, args = list(mean = m1, sd = std1))+
  stat_function(fun = dnorm, args = list(mean = m1, sd = std1), geom = "area", fill = "blue", alpha = 0.2)+
  stat_function(fun = dnorm, args = list(mean = m2, sd = std2))+
  stat_function(fun = dnorm, args = list(mean = m2, sd = std2), geom = "area", fill = "green", alpha = 0.2)+
  geom_vline(aes(xintercept = m1), col = "red")+
  geom_vline(aes(xintercept = m2), col = "red")+
  geom_vline(aes(xintercept = 0), alpha = 0.2) +
  scale_x_continuous(breaks = c(-5,0,m2,m1,5), labels = c(-5,0,expression(mu[B]),expression(mu[A]),5))+
  xlab(expression(paste("Distributions of ", r[A], " and ", r[B])))+
  ylab("Density")+
  ggtitle("")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_classic()


m = m1 - m2
std = sqrt(std1^2 + std^2)
ggplot(normal_df, aes(x = x)) + 
  stat_function(fun = dnorm, args = list(mean = m, sd = std)) + 
  stat_function(fun = dnorm, args = list(mean = m, sd = std), xlim = c(-1,1),geom = "area", fill = "purple", alpha = .2) +
  stat_function(fun = dnorm, args = list(mean = m, sd = std), xlim = c(-8,-1),geom = "area", fill = "red", alpha = .2)+
  stat_function(fun = dnorm, args = list(mean = m, sd = std), xlim = c(1,8),geom = "area", fill = "blue", alpha = .3) +
  xlab(expression(paste("Distribution of ",r[A]," - ", r[B])))+
  scale_x_continuous(breaks = c(-5,0,m1 - m2,5), labels = c(-5,0,expression(paste(mu[A]," - ", mu[B])),5))+
  ylab("Density")+
  theme(plot.title = element_text(hjust = 0.5))+
  annotate("text", x = -1.5, y = 0.02, label= "Loss ", size = 3)+
  annotate("text", x = 0.35, y = 0.059, label= "Draw ", size = 3)+
  annotate("text", x = 3.25, y = 0.06, label= "Win ", size = 3)+
  geom_vline(xintercept = 0, alpha = 0.2)+
  geom_vline(xintercept = m1 - m2, col = "red")+
  theme_classic()


hist_plot_df = data.frame(Update_Size = c(abs(womens_model_2021$update_size), abs(mens_model_2021$update_size)), League = c(rep("Womens",nrow(womens_model_2021)), rep("Mens", nrow(mens_model_2021))))

update_mean = hist_plot_df %>%
  group_by(League) %>%
  summarise(mean = mean(Update_Size))

g1 = ggplot(data = hist_plot_df, aes(x = Update_Size, group = League, col = League, fill = League))+
  geom_histogram(bins = 20,alpha = 0.1)+
  geom_vline(data = update_mean, aes(xintercept = mean, col = League))+
  xlab("Update Size")+
  ylab("")+
  ggtitle("")+
  scale_color_manual(values = c("blue", "hot pink"))+
  scale_fill_manual(values = c("blue", "hot pink"))+
  geom_segment(aes(x = 0.5, y = 500, xend = 0.5, yend = 1), arrow = arrow(length = unit(0.2, "cm")), col = "black")+
  annotate("text", x = 0.47, y = 575, label= "Upset Win", size = 3)+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))


g3 = ggplot(data = hist_plot_df, aes(x = Update_Size, group = League, col = League, fill = League))+
  geom_histogram(bins = 20,alpha = 0.1)+
  geom_vline(data = update_mean, aes(xintercept = mean, col = League))+
  xlab("Update Size")+
  ylab("")+
  ggtitle("")+
  scale_color_manual(values = c("blue", "hot pink"))+
  scale_fill_manual(values = c("blue", "hot pink"))+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))


rankings_dates_mens_2021 = c("2021-08-31", "2021-09-07", "2021-09-14", "2021-09-21", "2021-09-28", "2021-10-05", "2021-10-12", "2021-10-19", "2021-10-26", "2021-11-02", "2021-11-09", "2021-12-14")

rankings_dates_womens_2021 = c("2021-08-24", "2021-08-30", "2021-09-07", "2021-09-14", "2021-09-21", "2021-09-28", "2021-10-05", "2021-10-12", "2021-10-19", "2021-10-26", "2021-11-02", "2021-11-09", "2021-12-07")

calculate_ranks_deviation = function(data, ranking_dates){
  
  df = data
  team_deviation = c()
  
  for(j in 3:(length(ranking_dates)+1)){
    
    prior_vector = df %>% pull(colnames(df)[j-1])
    
    current_vector = df %>% pull(colnames(df)[j])
    
    value = abs(prior_rank - current_rank)
    
    team_deviation = c(team_deviation, value)
    
  }
  
  return(team_deviation)
}

ranks_deviation_womens_2021 = calculate_ranks_deviation(season_rankings_womens_2021,rankings_dates_womens_2021)

ranks_deviation_mens_2021 =calculate_ranks_deviation(season_rankings_mens_2021,rankings_dates_mens_2021)



hist_plot_df = data.frame(Deviation_Size = c(ranks_deviation_womens_2021,ranks_deviation_mens_2021),League = c(rep("Womens",length(ranks_deviation_womens_2021)), rep("Mens", length(ranks_deviation_mens_2021))))

dev_mean = hist_plot_df %>%
  group_by(League) %>%
  summarise(mean = mean(Deviation_Size, na.rm = TRUE))

g2 = ggplot(data = hist_plot_df, aes(x = Deviation_Size, group = League, col = League, fill = League))+
  geom_histogram(bins = 20,alpha = 0.1)+
  geom_vline(data = dev_mean, aes(xintercept = mean, col = League))+
  xlab("Rank Change")+
  ylab("")+
  ggtitle("")+
  scale_color_manual(values = c("blue", "hot pink"))+
  scale_fill_manual(values = c("blue", "hot pink"))+
  theme_classic()+
  geom_segment(aes( x = 75, y = 500, xend = 75, yend = 1), arrow = arrow(length = unit(0.2, "cm")), col = "black")+
  annotate("text", x = 75, y = 575, label= "Upset Win", size = 3)+
  theme(plot.title = element_text(hjust = 0.5))

g4 = ggplot(data = hist_plot_df, aes(x = Deviation_Size, group = League, col = League, fill = League))+
  geom_histogram(bins = 20,alpha = 0.1)+
  geom_vline(data = dev_mean, aes(xintercept = mean, col = League))+
  xlab("Rank Change")+
  ylab("")+
  ggtitle("")+
  scale_color_manual(values = c("blue", "hot pink"))+
  scale_fill_manual(values = c("blue", "hot pink"))+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

g1 + g2 + plot_layout(guides = "collect")


g3 + g4 + plot_layout(guides = "collect")
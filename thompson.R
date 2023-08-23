#thompson sampling

dataset = read.csv("Ads_CTR_Optimisation.csv")

#step1
N = 10000
d = 10

ads_selected = integer(d)
total_reward = 0

number_of_rewards_1 = integer(d)
number_of_rewards_0 = integer(d)


#step 2
for(n in 1:N){
  
  ad = 0
  max_random = 0
  
  for(i in 1:d){
    
    random_draws = rbeta(n=1,
                         shape1 = number_of_rewards_1[i]+1,
                         shape2 = number_of_rewards_0[i]+1)
    
    
    if( random_draws > max_random){
      max_random = random_draws
      ad = i
    }
  }
  
  
  ads_selected = append(ads_selected,ad)
  

  reward = dataset[n,ad]
  
  if(reward == 1){
    number_of_rewards_1[ad] = number_of_rewards_1[ad]+1
  }else{
    number_of_rewards_0[ad] = number_of_rewards_0[ad]+1
  }
  
  
  total_reward = total_reward + reward
  
}


#visualizing the histogram
hist(ads_selected,
     col = "blue",
     main = "historgram ads selection",
     xlab = "ads",
     ylab = "number of times ads were selected")
dataset = read.csv("Ads_CTR_Optimisation.csv")


#step1
N = 10000
d = 10

ads_selected = integer(d)
number_of_selections = integer(d)
sums_of_reward = integer(d)
total_reward = 0

#step 2
for(n in 1:N){
  
  ad = 0
  max_ub = 0
  
  for(i in 1:d){
    
    if(number_of_selections[i] > 0){
      
      avg_reward = sums_of_reward[i]/number_of_selections[i]
      delta_i = sqrt(((3/2)*log(n))/number_of_selections[i])
      
      upper_bound = avg_reward + delta_i
      
    }else{
      upper_bound = 1e400
    }
    
    
    if( upper_bound > max_ub){
      max_ub = upper_bound
      ad = i
    }
  }
  
  ads_selected = append(ads_selected,ad)
  number_of_selections[ad] = number_of_selections[ad] +  1
  reward = dataset[n,ad]
  sums_of_reward[ad] = sums_of_reward[ad] + reward
  
  total_reward = total_reward + reward
  
}


#visualizing the histogram
hist(ads_selected,
     col = "blue",
     main = "historgram ads selection",
     xlab = "ads",
     ylab = "number of times ads were selected")





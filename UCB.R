dataset = read.csv("Ads_CTR_Optimisation.csv")

N = 10000
d = 10
ads_selected = integer(0)
total_reward = 0

for(n in 1:N){
  ad = sample(1:10,1)
  ads_selected = append(ads_selected,ad)
  reward = dataset[n,ad]
  total_reward = total_reward + reward
  
}

hist(ads_selected,
     col = "blue",
     main = "historgram ads selection",
     xlab = "ads",
     ylab = "number of times ads were selected")
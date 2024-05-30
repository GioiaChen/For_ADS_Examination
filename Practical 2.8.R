# Is the Guinness factory adding enough barley?

# The Guinness beer factory requires 50 g of barley per pint of beer. 
# They examined 50 pints and found an average barley content of 46 g per pint.

# 1. Use my priors as described below to determine the posterior probability that enough barley is being added to each pint.
# P(H1) = P(enough barley) = 0.5
# P(H2) = P(not enough barley) = 0.5
# P(DATA|H1) = P(mean barley content 46g|enough barley) = 0.7
# P(DATA|H2) = P(mean barley content 46g|not enough barley) = 0.4
# What is the Bayes Factor for these two hypotheses?
ph1=0.5
ph2=0.5
pdh1=0.7
pdh2=0.4

bayes_factor = (pdh1 * ph1) / (pdh2 * ph2)

# 2.Lets explore the probability of seeing the data given each of the two hypotheses. 
# The numbers given here are estimates based on experience. 
# To see how they influence the Bayes factor, 
# calculate the Bayes Factor 
# for each combination of P(DATA|H1)and P(DATA|H2) from 0 to 1 in steps of 0.1.
# Please plot these on a graph where these two probabilities are the x and y axes.
bayes_factors <- as.data.frame(matrix(nrow=10,ncol=10))
for (i in 1:10) {
  for (j in 1:10) {
    bayes_factors[i,j] <- i/j
  }
}

ggplot(data=)

# Now, I think that my staff are stealing barley to make their own moonshine Guiness at home. 
# I update my first prior to P(A) = P(enough barley) = 0.2. 
# How does this affect the Bayes Factor? How does it affect the posteriors?
ph1_1=0.2
ph2_1=0.8
pD=0.2*0.7+0.8*0.4
bayes_factor_1=0.2*0.7/(0.8*0.4)
ph1d_1=0.7*0.2/0.46
ph2d_1=0.4*0.8/0.46


# A dice game
# You play a dice-based game with a friend online, using six-sided dice.
# In this game, rolling a six is especially lucky and wins you many points.
# Since you are playing via video link, your friend uses their own die.
# You can see that it is six-sided, but you cannot see the numbers clearly.
# Over the course of the game, your friend rolls the die 20 times, and 7 out of those 20 rolls are sixes.
# Do you think your friend is playing fair?

# How many sixes do you believe their die has? 1 (like a normal die)? 
# Or more (the die is unfair)?

# How? Before you start coding, think about this for a moment. 

# The main idea is you have 6 possible hypotheses, one for each number of sixes.
# Is it really 6 though? Also, how do you compare these hypothesis?
# Do you have to compare every hypothesis with every other?

pro<- data.frame(numberOfSix=c(1:6),
                 proh=c(replicate(5,0.2),0),
                 prodh=numeric(6),
                 bf=numeric(6))
for (i in 1:6) {
  pro[i,3]=(choose(20,7))*((i/6)^7)*(((6-i)/6)^13)
  pro[i,4]=pro[1,3]/pro[i,3]
}
round(pro[,3],digits=8)
round(pro[,4],digits=5)
# The case with 2 sixes in the dice has the smallest posterior ratio, indicating to the highest p(H|DATA).


















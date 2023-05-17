# Introduction to Bayesian Data Analysis
# Assignment 1

library(dplyr)

### 2 Dice ###

#1
#Creates two vectors that represent the faces of a dice
d1 <- c(1, 2, 3, 4, 5, 6)
d2 <- c(1, 2, 3, 4, 5, 6)

#2
#Creates a data frame with the two created vectors and assign a name to each column
dice_outcomes <- expand.grid(d1 = d1, d2 = d2)
dice_outcomes

#3
#Creates the vector "probability" which has the JOINT probabilities calculated by using the number of rows
probability <- rep(1 / nrow(dice_outcomes), nrow(dice_outcomes))

#adds the probability vector to the data frame that was created.
dice_outcomes_withProb <- data.frame(dice_outcomes, probability)
dice_outcomes_withProb

#4
# Creates a function that sums the columns of a data frame
summing <- 
  function(x){
    
    total <- 0
    for (i in 1:length(x)) {
      total <- total + x[i]
    }
    return(total)
  }

# adds the sums directly to the data frame and rename the added column for better format.
# notice that for the sum we use the data frame that only contains the dice columns.
dice_outcomes_withProb <- data.frame(dice_outcomes_withProb, summing(dice_outcomes)) 

#rename for better presentation.
dice_outcomes_withProb <- rename(dice_outcomes_withProb, sum = d1.1)
dice_outcomes_withProb

#5
# Since this exercise asks us for a conditional probability P(sum>=7 | d1 = 3), we need to calculate 
# the marginal probability of subset P(d1=3) and the joint probability of subset P(d1  = 3 ∩ sum>=7)

#Calculate joint probability of subset P(d1  = 3 ∩ sum>=7)
subset_jointProbability <- subset(dice_outcomes_withProb, d1 == 3 & sum >= 7)

#Calculate marginal probability of subset P(d1=3)
subset_margProbability <- subset(dice_outcomes_withProb, d1 == 3)

#Calculate conditional probability P(sum>=7 | d1 = 3)
conditionalProbability <- sum(subset_jointProbability$probability) / sum(subset_margProbability$probability)
conditionalProbability

#6
# Calculate the joint probability of this event by creating a subset that complies with both
# conditions and summing the probabilities in this subset
subset_jointProbability_2 <- subset(dice_outcomes_withProb, sum >= 4 & sum <= 9)
probability_2 <- sum(subset_jointProbability_2$probability)
probability_2

#7
# First we subset the data to get the rows with the most repeated sum.
rowsMaxSum <- tail(names(sort(table(dice_outcomes_withProb$sum))),1)

# Then we sum the joint probability of each of these rows.
subset_mostSum <- subset(dice_outcomes_withProb, sum == rowsMaxSum)
mostProbableSum <- sum(subset_mostSum$probability)
mostProbableSum


### Probability of Delay ###

#8
# Generate all the probability distribution vectors for each for each possible probability of delay.
prob_0.0 <- dbinom(0:10, size = 10, prob = 0.0)
prob_0.1 <- dbinom(0:10, size = 10, prob = 0.1)
prob_0.2 <- dbinom(0:10, size = 10, prob = 0.2)
prob_0.3 <- dbinom(0:10, size = 10, prob = 0.3)
prob_0.4 <- dbinom(0:10, size = 10, prob = 0.4)
prob_0.5 <- dbinom(0:10, size = 10, prob = 0.5)
prob_0.6 <- dbinom(0:10, size = 10, prob = 0.6)
prob_0.7 <- dbinom(0:10, size = 10, prob = 0.7)
prob_0.8 <- dbinom(0:10, size = 10, prob = 0.8)
prob_0.9 <- dbinom(0:10, size = 10, prob = 0.9)
prob_1.0 <- dbinom(0:10, size = 10, prob = 1.0)

# Add each vector to a data frame and rename the rows to 0:10 to represent the number of delays.
ProbDist_DataFrame <- data.frame(prob_0.0, prob_0.1, prob_0.2, prob_0.3, prob_0.4, prob_0.5, 
                                 prob_0.6, prob_0.7, prob_0.8, prob_0.9, prob_1.0)

# Renames rows to represent the numbers of delays
row.names(ProbDist_DataFrame) <- 0:10
ProbDist_DataFrame

#9
# A function is created to simulate delays of trains 
sim_rides <- function(N, p){
  sample(c("L", "O"), size=N, replace=TRUE, prob=c(p, 1-p))
}
# the probability of the delay is .3, and running 10 scenarios 
set.seed(1237)
obs <- sim_rides(10, .3)
obs

# it was obtained 5 delays, then we use dbinom function to calculate the likelihood 
# of the data considering all the different probabilities of delay, from 0 to 1 by .10.
likelihood_0 <- dbinom(sum(obs == "L"), size = 10, 0)
likelihood_0.1 <- dbinom(sum(obs == "L"), size = 10, .1)
likelihood_0.2 <- dbinom(sum(obs == "L"), size = 10, .2)
likelihood_0.3 <- dbinom(sum(obs == "L"), size = 10, .3)
likelihood_0.4 <- dbinom(sum(obs == "L"), size = 10, .4)
likelihood_0.5 <- dbinom(sum(obs == "L"), size = 10, .5)
likelihood_0.6 <- dbinom(sum(obs == "L"), size = 10, .6)
likelihood_0.7 <- dbinom(sum(obs == "L"), size = 10, .7)
likelihood_0.8 <- dbinom(sum(obs == "L"), size = 10, .8)
likelihood_0.9 <- dbinom(sum(obs == "L"), size = 10, .9)
likelihood_1 <- dbinom(sum(obs == "L"), size = 10, 1)

# Create a vector of likelihoods
likelihoods <- c(likelihood_0, likelihood_0.1, likelihood_0.2, likelihood_0.3, likelihood_0.4, likelihood_0.5,
                 likelihood_0.6, likelihood_0.7, likelihood_0.8, likelihood_0.9, likelihood_1)
likelihoods

#10
# Creates prior probabilities vector
prior <- c(0.000, 0.004, 0.041, 0.123, 0.209, 0.246, 0.209, 0.123, 0.041, 0.004, 0.000)

# Function that creates a table with the prior, likelihoods, posterior and posterior norm
compute_post <- function(likelihoods, prior){
  #Bayes' rule
  posterior <- likelihoods * prior
  posterior_norm <- posterior / sum(posterior)
  tibble(prior, likelihoods, posterior, posterior_norm)
}
# Runs function
compute_post(likelihoods, prior)


## Call centre modelling using the Erlang C formula

# The Erlang C formula is used to calculate the chance of queued calls 
# (waiting to be answered). This can be used to determine how many 
# operators/agents are needed to answer all arriving calls.

# The Erlang C formula does not consider call abandonment: 
# it assumes that all callers will remain in the queue until they 
# are being served. In reality, some callers will abandon when the 
# waiting time exceeds their patience. This has the effect of improving 
# the waiting time for subsequent callers in the queue. Abandonment will 
# therefore improve the service level for remaining callers. The net effect 
# of this is, that the Erlang C formula will essentially overestimate 
# the numbers of operators/agents that are needed.

# Functions related to (contact center) staffing,
# using the Erlang C function

# Terms used:
# rate-- Number of arrivals per interval
# duration-- Average handling time in seconds
# interval-- Length of interval in minutes (default = 60)
# agents-- Number of available agents
# target -- Acceptable waiting time
# gos_target-- Service level goal, the percentage of calls answered 
# within the acceptable waiting time

# Calculate traffic intensity/workload
# Return--  Traffic intensity in Erlangs
intensity <- function(rate, duration, interval = 60) {
  (rate / (60 * interval)) * duration
}

# Calculate the chance of a queued call (Erlang C function)
# Return-- Chance of queueing
erlang_c <- function(agents, rate, duration, interval = 60) {
  int <- intensity(rate, duration, interval)
  erlang_b_inv <- 1
  for (i in 1:agents) {
    erlang_b_inv <- 1 + erlang_b_inv * i / int
  }
  erlang_b <- 1 / erlang_b_inv
  agents * erlang_b / (agents - int * (1 - erlang_b))
}

# Calculate the servicelevel 
# Calculates the percentage of calls that are answered within the 
# acceptable waiting time
# Return-- Service level (% of calls answered within acceptable waiting time)
service_level <- function(agents, rate, duration, target, interval = 60) {
  pw <- erlang_c(agents, rate, duration, interval)
  int <- intensity(rate, duration, interval)
  1 - (pw * exp(-(agents - int) * (target / duration)))
}

# Calculate the number of needed agents for SL goal
# Calculates the number of agents that are needed to achieve a required service level. Currently only
# calculates a whole (integer) number of agents.
# Return-- Number of agents needed to achieve service level
resource <- function(rate, duration, target, gos_target, interval = 60) {
  agents <-round(intensity(rate, duration, interval) + 1)
  gos <- service_level(agents, rate, duration, target, interval)
  while (gos < gos_target * (gos_target > 1) / 100) {
    agents <- agents + 1
    gos <- service_level(agents, rate, duration, target, interval)
  }
  return(c(agents, gos))
}

# Example 01: Test scenario where 
# Number of arrivals per interval: 100, 
# Duration of each call is 180,
# Acceptable waiting time is 20 sec, 
# Service Level goals, the percentage of calls answered is 90%,
# Length of interval in minutes is 30 sec
resource(100, 180, 20, 90, 30)

# output is [1] 15.0000000  0.9414528
# The Number of resources required to meet the workload is 15 agents
# with 94% of calls getting answered

## Monte Carlo Simulation
# Example 02: 
# a call centre receives on average 100 calls per half hour 
# with a standard deviation of 10 calls. The average time to manage a call, 
# including wrap-up time after the call, is 180 seconds with a standard 
# deviation of 20 seconds. The centre needs to answer 80% of calls within 
# 20 seconds. What is the likelihood of achieving this level of service?
library(tidyverse)
library(dplyr)

# Ressource required 
resource(100, 180, 20, 80, 30)
# output is [1] 14.00000  0.88835, i.e. 14 agents are required to meet SL goals

intensity_mc <- function(rate_m, rate_sd, duration_m, duration_sd, interval = 60, sims = 1000) {
  (rnorm(sims, rate_m, rate_sd) / (60 * interval)) * rnorm(sims, duration_m, duration_sd)
}

# Resources required Monte Carlo Simulation
intensity_mc(100, 10, 180, 20, interval = 30) %>% summary()
# output is
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 5.720   8.896   9.920   9.978  11.030  14.391 
# Simulating the intensity of the scenario 1000 times suggests we need 
# between 6 and 16 agents to manage this workload.

erlang_c_mc <- function(agents, rate_m, rate_sd, duration_m, duration_sd, interval = 60) {
  int <- intensity_mc(rate_m, rate_sd, duration_m, duration_sd, interval)
  erlang_b_inv <- 1
  for (i in 1:agents) {
    erlang_b_inv <- 1 + erlang_b_inv * i / int
  }
  erlang_b <- 1 / erlang_b_inv
  agents * erlang_b / (agents - int * (1 - erlang_b))
}

service_level_mc <- function(agents, rate_m, rate_sd, duration_m, duration_sd, target, interval = 60, sims = 1000) {
  pw <- erlang_c_mc(agents, rate_m, rate_sd, duration_m, duration_sd, interval)
  int <- intensity_mc(rate_m, rate_sd, duration_m, duration_sd, interval, sims)
  1 - (pw * exp(-(agents - int) * (target / rnorm(sims, duration_m, duration_sd))))
}

data_frame(ServiceLevel = service_level_mc(agents = 12,
                                           rate_m = 100,
                                           rate_sd = 10,
                                           duration_m = 180,
                                           duration_sd = 20,
                                           target = 20,
                                           interval = 30,
                                           sims = 1000)) %>%
  ggplot(aes(ServiceLevel)) +
  geom_histogram(binwidth = 0.1, fill = "#008da1")

service_level_mc(15, 100, 10, 180, 20, 20, 30, sims = 1000) %>%
  quantile(c(.05, .5, .95))

# The output is 
#        5%       50%       95% 
# 0.7412067 0.9413291 0.9912122 

# The last step is to simulate the expected service level for this scenario. 
# The plot visualises the outcome of the Monte Carlo simulation and shows 
# that 95% of situations the Grade of Service is more than 77% and 
# half the time it is more than 94%.
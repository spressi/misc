library(tidyverse)

rollW6s = function(diceN) { runif(diceN, 1, 7) %>% floor() }
rollsToActions = function(rolls, safeDice) { rolls + 2 * safeDice }
rollsToActionsPeterman = function(rolls, safeDice) { 
  safeDiceActions = ifelse(safeDice <= 1, 0, 2 * (safeDice-1))
  return(rolls + safeDiceActions)
}

safeDice = 1:6 #number of safe dice before starting to roll (max 6)
toThrows = 3:6 #number of different numbers that need to be rolled
iterationsPerCell = 1E5

results = tibble(dicePerRoll = 1:2, numsToRoll = 1:2, rolls.m = 0.0, rolls.sd = 0.0) %>% filter(F)
for (n in safeDice) {
  for (m in toThrows) {
    interim = c()
    
    for (i in 1:iterationsPerCell) {
      toThrow = 1:m
      throws = 0
      
      while (toThrow %>% length() != 0) {
        toThrow = toThrow %>% setdiff(rollW6s(n))
        throws = throws + 1
      }
      interim = interim %>% c(throws)
    }
    
    results = results %>% bind_rows(tibble(dicePerRoll = n, numsToRoll = m, 
                                           rolls.m = interim %>% mean(), rolls.sd = interim %>% sd()))
  }
}

results = results %>% mutate(actions.m = rollsToActions(rolls.m, dicePerRoll),
                             #actions.m = rollsToActionsPeterman(rolls.m, dicePerRoll),
                             actions.sd = rolls.sd, #actions.sd = rollsToActions(rolls.sd, dicePerRoll), #the uncertainty of actions only relates to the uncertainty of rolls
                             safeDiceN = as_factor(dicePerRoll), differentNums = as_factor(numsToRoll)) %>% 
  group_by(differentNums) %>% mutate(minimum = actions.m == min(actions.m))
results %>% ggplot(aes(x = safeDiceN, y = actions.m, color=differentNums, group=differentNums, shape = minimum)) +
  geom_ribbon(aes(ymin = actions.m - actions.sd, ymax = actions.m + actions.sd, fill=differentNums), alpha=.2) + 
  geom_line() + geom_point(aes(size = minimum)) +
  scale_color_viridis_d() + scale_fill_viridis_d() + theme_bw()

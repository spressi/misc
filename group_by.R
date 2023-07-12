library(tidyverse)

summary1 = mpg %>% 
  group_by(manufacturer, model) %>% 
  summarise(across(c(cyl, cty, hwy), mean)) %>% 
  mutate(across(c(cyl, cty, hwy), function(x) scale(x)[,1], .names="{.col}_z"))
#problem: grouping is still active
#z-standardization occurs separately for each manufacturer!

summary2 = mpg %>% 
  group_by(manufacturer, model) %>% 
  summarise(across(c(cyl, cty, hwy), mean)) %>% 
  ungroup() %>% #solution 1: ungroup prior to calling mutate
  mutate(across(c(cyl, cty, hwy), function(x) scale(x)[,1], .names="{.col}_z_correct"))

full_join(summary1, summary2) %>% 
  select(manufacturer, model, order(tidyselect::peek_vars())) #order nicely


#better solution:
mpg %>% 
  #group_by(manufacturer, model) %>% #avoid group_by
  summarise(across(c(cyl, cty, hwy), mean),
            .by=c(manufacturer, model)) %>% #group in summarise
  mutate(across(c(cyl, cty, hwy), function(x) scale(x)[,1], .names="{.col}_z"))

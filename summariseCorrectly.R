library(tidyverse)

se = function(x, na.rm = FALSE) {
  sd(x, na.rm) / sqrt(if(!na.rm) length(x) else sum(!is.na(x)))
}

N = 40 #total number of subjects (needs to be an even number for split across between conditions)


#set up
data = tibble(subject = rep(1:N, each=2), #two lines per subject due to within variable
              iv.between = rep(1:2, N/2, each=2), #manipulation/effect of interest
              iv.within = rep(1:2, N), #a within-subject control variable like target side in a gaze cueing experiment
              dv = rnorm(N*2, mean=subject, sd=100)) #add variability
head(data) %>% bind_rows(tail(data)) %>% print() #look at start & end of data frame


#analysis
average1 = data %>% group_by(iv.between) %>% summarise(dv.m = mean(dv), dv.sd = sd(dv), dv.se = se(dv), dv.n = n())
average2 = data %>% group_by(iv.between, subject) %>% #make sure subject is last level!
  summarise(dv.m.subject = mean(dv), dv.n = n()) %>% #one value per factor level (combination) and subject
  summarise(dv.m = mean(dv.m.subject), dv.sd = sd(dv.m.subject), dv.se = se(dv.m.subject), dv.n = n()) #summarise across subjects (one value per subject => correct n)

#compare results
#full_join(average1, average2, by="iv.between", suffix = c("_1", "_2")) %>% select(iv.between, order(tidyselect::peek_vars())) %>% print()
bind_rows(average1 %>% mutate(method="summarise once"), 
          average2 %>% mutate(method="summarise twice")) %>% 
  select(method, everything()) %>% arrange(iv.between) %>% mutate(dv.n.correct = dv.n==N/2) %>% print()
#means are identical
#SD is smaller for method 2 due to regression to the mean while averaging across iv.within
#SE is usually larger for method 2 because correct n is used
#correct is dv.n == N/2 per group (method 2)

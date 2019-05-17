

library(tidyverse)


##############################Note###################################
#make sure you analyse the test results on the split level. 
#In other words, if your A/B split happen on the user level, you should group metrics per user not per transaction
#####################################################################


############function defenitions################
boot_mean = function(x, iterations = 1000){
  samples_means = numeric(iterations)
  for (i in 1:iterations){
    samp = sample(x, size = length(x), replace = TRUE)
    samples_means[i] = mean(samp, na.rm = T)
  }
  return(samples_means)
}


boot_prop = function(x, iterations = 1000, target = 1){
  samples_props = numeric(iterations)
  for (i in 1:iterations){
    samp = sample(x, size = length(x), replace = T)
    samples_props[i] = length(samp[samp == target])/length(samp)
  }
  return(samples_props)
}

#input is boot_prop/boot_mean results. Output is df with confidence interval of mean
ci_boot = function(x, ci = 0.95) {
  lb = quantile(x, (1-ci)/2)
  ub = quantile(x, 0.95 + (1-ci)/2)
  median = median(x)
  tibble = tibble(lb, median, ub)
  names(tibble) = c(names(lb), "median", names(ub))
  return(tibble)
}

#works for both prop and mean. Prop should be 0 and 1.returns the % increase
ab_boot = function(treatment, control, iterations = 1000, ci = 0.95) {
  treatment_boot = boot_mean(treatment, iterations)
  control_boot = boot_mean(control, iterations)
  ab_results = ci_boot(treatment_boot - control_boot)/control_boot
  return(ab_results)
}


boot_hypoth = function(x, y, iterations = 1000) {
  mean_diff = abs(mean(x, na.rm = T) - mean(y, na.rm = T))
  all = c(x, y)
  diff = numeric(iterations)
  for (i in 1:iterations){
    temp = sample(all, size = length(all), replace = T)
    mean1 = mean(temp[1:length(x)], na.rm = T)
    mean2 = mean(temp[(length(x)+1):length(all)], na.rm = T)
    diff[i] = abs(mean1 - mean2)
  }
  pvalue = length(diff[diff >= mean_diff])/length(diff)
  return(pvalue)
}


boot_hypoth_prop = function(x, y, iterations = 1000, target = 1) {
  prop_diff = abs(length(x[x==target])/length(x) - length(y[y==target])/length(y))
  all = c(x, y)
  diff = numeric(iterations)
  for (i in 1:iterations){
    temp = sample(all, size = length(all), replace = T)
    group1 = temp[1:length(x)]
    group2 = temp[(length(x)+1):length(all)]
    prop1 = length(group1[group1==target])/length(group1)
    prop2 = length(group2[group2==target])/length(group2)
    diff[i] = abs(prop1 - prop2)
  }
  pvalue = length(diff[diff >= prop_diff])/length(diff)
  return(pvalue)
}
######################################################





##############################Note###################################
#make sure you analyse the test results on the split level. 
#In other words, if your A/B split happen on the user level, you should group metrics per user not per transaction
#####################################################################

################Example########################################
control = df %>% filter(variant == "control")
treatment = df %>% filter(variant == "treatment")

metrics = bind_rows(
  bind_cols(metric = "aaa", 
            ab_boot(treatment$aaa, control$aaa)),
  bind_cols(metric = "is_bbb", 
            ab_boot(treatment$is_bbb, control$is_bbb)))

metrics
metrics %>%
  ggplot(aes(x = metric, y = median, ymin = `2.5%`, ymax = `97.5%`)) + 
  geom_pointrange(color = "dark blue") + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(y = "% change") + 
  theme_minimal() +
  theme(panel.grid.major = element_line(size = 0.2),
        panel.grid.minor = element_line(size = 0.2),
        panel.grid.major.x = element_line(NA))


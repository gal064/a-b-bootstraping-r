# A/B Testing Bootstrapping functions - R 

This repository contains an R script with functions to conduct bootstrapping for A/B testing purposes.
The functions are to calculate confidence interval of mean and proportion and do hypotheses testing between groups.
There is also an example of how to use the functions to analyze a user split test.

Example:
```




set.seed(123)
#creating an example data
df <- rbind(
  tibble(customer_id = 1:100000,
             response = sample(c(0,1), size = 100000, replace = TRUE, prob = c(0.93, 0.07)),
             purchase_size = rnorm(100000, mean = 100, 200)*response, 
             variant = "control"),
  tibble(customer_id = 100001:200000,
             response = sample(c(0,1), size = 100000, replace = TRUE, prob = c(0.925, 0.075)),
             purchase_size = rnorm(100000, mean = 103, 200)*response, 
             variant = "treatment"))

#splitting to control vs treatment
control <- df %>% filter(variant == "control")
treatment <- df %>% filter(variant == "treatment")

#calculating the 95% confidence interval of response rate and avg purchase size for each group
ci_boot(boot_mean(control$response))
ci_boot(boot_mean(treatment$response))
ci_boot(boot_mean(control$purchase_size[control$response == 1]))
ci_boot(boot_mean(treatment$purchase_size[treatment$response == 1]))

#calculating and visualizing the difference in response rate and avg purchase between groups
metrics <- bind_rows(
  bind_cols(metric = "avg_conversion", 
            ab_boot(treatment$response,
                    control$response)),
  bind_cols(metric = "avg_purchase_size", 
            ab_boot(treatment$purchase_size[treatment$response == 1],
                    control$purchase_size[control$response == 1])))

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

#we can see by using the graph the % lift in conversion is significant, but the % luft in avg purchase size, among those who converted is not significant
```

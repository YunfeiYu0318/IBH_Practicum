# ===== HELPER FUNCTIONS ===== #
library(dplyr)
library(tidyverse)

# Check missing value count and percentages for scales
get_missing_stats <- function(data, prefix, caption){
  # calculate missing stats
  raw.data %>% 
    select(starts_with(prefix)) %>% 
    summarise(
      across(everything(),
             list(
               count = ~sum(is.na(.)),
               pct = ~mean(is.na(.))*100,
               comp = ~mean(!is.na(.))*100
             ))) %>% 
    pivot_longer(
      everything(),
      names_to = c("item", ".value"),
      names_pattern = "(.*)_(count|pct|comp)"
    ) %>% 
    # create html table
    kable(digits = 2,
          col.names = c("scale item", "n missing", "missing rate", "completion rate"),
          caption = caption,
          format = "html") %>% 
    kable_styling(bootstrap_options = c("striped", "hover"))
}


# Plot distribution for scale items
plt_scale_dist <- function(data, prefix, num_col = 3){
  data %>% 
    select(starts_with(prefix)) %>% 
    pivot_longer(
      cols = everything(),
      names_to = "item",
      values_to = "score"
    ) %>% 
    mutate(score = factor(score)) %>% 
    ggplot(aes(x = score)) +
    geom_bar(fill = "royalblue3", color = "white") +
    facet_wrap(~item, ncol = num_col) +
    theme_minimal() +
    labs(x = "Score", y = "Count")
}
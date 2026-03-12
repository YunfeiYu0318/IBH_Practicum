# ===== HELPER FUNCTIONS ===== #
library(dplyr)
library(tidyverse)
library(forcats)


# ----- SUMMARY TABLES AND PLOTS ----- #

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


# Summary stats for categorical variables (n and percentage)
# get_category_summary <- function(data, caption, id = NULL){
#   
#   if(!is.null(id)){
#     data <- data %>% select(-any_of(id))
#   }
#   
#   data %>% 
#     mutate(across(where(is.factor), ~fct_na_value_to_level(.x, level = "Missing"))) %>% 
#     tbl_summary(
#       missing = "no", # NAs treated as an explicit category
#       # missing_text = "Missing",
#       # missing_stat = "{n} ({p}%)",
#       statistic = list(
#         all_categorical() ~ "{n} ({p}%)"
#       )
#     ) %>% 
#     bold_labels() %>% 
#     modify_caption(caption) %>% 
#     as_gt() %>% 
#     tab_options(table.width = pct(100))
# }

get_category_summary <- function(data, caption, id = NULL){
  
  if(!is.null(id)){
    data <- data %>% select(-any_of(id))
  }
  
  data %>% 
    mutate(across(everything(), ~fct_na_value_to_level(.x, level = "Missing"))) %>% 
    tbl_summary(
      missing = "no", # NAs treated as an explicit category
      # missing_text = "Missing",
      # missing_stat = "{n} ({p}%)",
      statistic = list(
        all_categorical() ~ "{n} ({p}%)"
      )
    ) %>% 
    bold_labels() %>% 
    modify_caption(caption) %>% 
    as_gt() %>% 
    tab_options(table.width = pct(100))
}



















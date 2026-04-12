# ===== HELPER FUNCTIONS ===== #
library(dplyr)
library(tidyverse)
library(forcats)



# ----- USING VARIABLE SUMMARY ----- #

# Extract variables of the given category from the summary df
extract_vars <- function(cat, summary = variable_summary){
  data <- summary %>% 
    filter(category == cat) %>% 
    select(variable)
  return(data$variable)
}

# ----- SUMMARY TABLES AND PLOTS ----- #

# Check missing value count and percentages for scales
get_missing_stats <- function(data, caption = "Missing percentage summary", prefix = NULL, export_df = FALSE){
  
  # Handle prefix filtering
  if(!is.null(prefix)){
    data <- data %>% dplyr::select(dplyr::starts_with(prefix))
  }
  
  # Calculate stats
  stats_df <- data %>% 
    dplyr::summarise(
      dplyr::across(dplyr::everything(),
                    list(
                      count = ~sum(is.na(.)),
                      pct = ~mean(is.na(.)) * 100,
                      comp = ~mean(!is.na(.)) * 100
                    ))) %>% 
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = c("item", ".value"),
      names_sep = "_(?=[^_]+$)" # Splits at the LAST underscore only
    )
  
  # Conditional Return
  if(export_df) {
    stats_df <- stats_df %>% 
      rename("variable" = "item",
             "missing_n" = "count",
             "missing_pct" = "pct",
             "completion_pct" = "comp")
    
    return(stats_df)
  }
  
  # Create HTML table (if export_df is FALSE)
  stats_df %>% 
    knitr::kable(digits = 2,
                 col.names = c("Scale Item", "N Missing", "Missing Rate (%)", "Completion Rate (%)"),
                 caption = caption,
                 format = "html") %>% 
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover"))
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
get_category_summary <- function(data, caption = NULL, id = NULL){
  
  if(!is.null(id)){
    data <- data %>% select(-any_of(id))
  }
  
  data %>%
    mutate(across(everything(), as.factor)) %>% 
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

# Summary stats for checkbox variables (n and percentage)

# get_checkbox_summary <- function(data, prefix, colname, caption = NULL, id = NULL){
#   # colname = the name for the first column
#   if(!is.null(id)){
#     data <- data %>% select(-any_of(id))
#   }
#   
#   data %>% 
#     select(starts_with(prefix)) %>% 
#     summarise(across(everything(), list(
#       # Calculate Count (n)
#       n = \(x) sum(x == 1, na.rm = TRUE),
#       # Calculate Percentage
#       Perc = \(x) round(mean(x, na.rm = TRUE), 4) * 100
#     ))) %>% 
#     pivot_longer(
#       everything(),
#       names_to = c("Category", ".value"),
#       names_pattern = paste0(prefix, "(.*)_(Perc|n)")
#     ) %>%
#     # Clean up the category names (remove leading underscores if they exist)
#     mutate(Category = str_remove(Category, "^_")) %>%
#     arrange(desc(Perc)) %>% 
#     kable(digits = 2,
#           col.names = c(colname, "Group Size", "Frequency (%)"),
#           caption = caption,
#           format = "html") %>% 
#     kable_styling(bootstrap_options = c("striped", "hover"))
# }

# extract stats from reliability output

extract_alpha_results <- function(alpha_obj, scale_name) {
  # Extract Feldt CI
  if (!is.null(alpha_obj$feldt)) {
    low <- round(alpha_obj$feldt$lower.ci, 2)
    up  <- round(alpha_obj$feldt$upper.ci, 2)
    ci_label <- paste0("(", low, ", ", up, ")")
  } else {
    ci_label <- "(N/A)"
  }
  
  # Extract other stats
  alpha_val <- round(alpha_obj$total$raw_alpha, 2)
  avg_r <- round(alpha_obj$total$average_r, 2)
  mean_val <- round(alpha_obj$total$mean, 2)
  sd_val <- round(alpha_obj$total$sd, 2)
  
  # Final Output Table
  tibble(
    `Scale Name` = scale_name,
    `N Items` = alpha_obj$nvar,
    `Raw Alpha (95% CI)` = paste0(alpha_val, " ", ci_label),
    `Average r` = avg_r,
    `Mean (SD)` = paste0(mean_val, " (", sd_val, ")")
  )
}















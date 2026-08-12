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


extract_item_stats <- function(data, alpha_obj, scale_prefix = NULL, cap = NULL){
  # get the item names from the alpha object
  item_names <- rownames(alpha_obj$item.stats)
  
  if(!is.null(scale_prefix)){
    data <- data %>% select(starts_with(scale_prefix))
  }
  # describe distribution for each item
  sum <- psych::describe(data) %>% 
    as.data.frame() %>% 
    rownames_to_column("item")
  
  # extract stats from alpha_obj
  item_stats_df <- alpha_obj$item.stats %>%
    as.data.frame() %>%
    rownames_to_column("item") %>%
    select(item, r.drop)
  
  alpha_drop_df <- alpha_obj$alpha.drop %>%
    as.data.frame() %>%
    rownames_to_column("item") %>%
    dplyr::select(item, raw_alpha) %>%
    rename(alpha_if_dropped = raw_alpha)
  
  # join components
  final_item_table <- sum %>%
    left_join(item_stats_df, by = "item") %>%
    left_join(alpha_drop_df, by = "item") 
  
  # printing summary table
  final_item_table %>% 
    mutate(
      mean_sd = paste0(round(mean, 2), " (", round(sd, 2), ")"),
      med_mad = paste0(median," (", round(mad, 2), ")"),
      range = paste0("(", min, ", ", max, ")")
    ) %>%
    select(item,
           `Valid n` = n,
           `Mean (SD)` = mean_sd,
           `Median (MAD)` = med_mad,
           Range = range,
           Skew = skew,
           Kurtosis = kurtosis,
           `Corrected Item-Total r` = r.drop,
           `Alpha if Dropped` = alpha_if_dropped) %>% 
    kable(format = "html",
          caption = cap, 
          digits = 2) %>%
    kable_styling(bootstrap_options = c("striped", "hover")) %>% 
    print()
  
  return(invisible(final_item_table))
}

# ---------- CFA Metrics ------------- #
get_fit_metrics <- function(fit_obj, model_name) {
  metrics <- fitMeasures(fit_obj, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))
  data.frame(
    Model = model_name,
    Chisq = round(metrics["chisq"], 3),
    Df = metrics["df"],
    CFI = round(metrics["cfi"], 3),
    TLI = round(metrics["tli"], 3),
    RMSEA = round(metrics["rmsea"], 3),
    SRMR = round(metrics["srmr"], 3)
  )
}


# ----------- LPA Results and Plots ------------- #

plot_lpa_profiles <- function(lpa_model, plot_title = "Latent Profile Analysis") {
  
  # Extract the means matrix from the underlying mclust model object
  if (!"model" %in% names(lpa_model) || !"parameters" %in% names(lpa_model$model)) {
    stop("The provided object does not appear to be a valid tidyLPA/mclust model slot.")
  }
  
  means_matrix <- lpa_model$model$parameters$mean
  
  # Reshape the matrix into a tidy dataframe for ggplot
  tidy_plot_data <- as.data.frame(means_matrix) %>%
    rownames_to_column(var = "Indicator") %>%
    pivot_longer(
      cols = -Indicator, 
      names_to = "Profile", 
      values_to = "Mean_Score"
    ) %>%
    mutate(Profile = str_remove(Profile, "V")) # Converts "V1", "V2" to "1", "2"
  
  # Generate the clean line plot
  ggplot(tidy_plot_data, aes(x = Indicator, y = Mean_Score, group = Profile, color = Profile)) +
    geom_line(aes(linetype = Profile), size = 1.2) + 
    geom_point(aes(shape = Profile), size = 1.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray60", alpha = 0.7) +
    scale_y_continuous(limits = c(-2.5, 2.5), breaks = seq(-2, 2, by = 0.5)) + 
    labs(
      title = plot_title,
      x = "Factor scores",
      y = "Std. Mean Score",
      color = "Profile",
      shape = "Profile",
      linetype = "Profile"
    ) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      text = element_text(size = 10),
      axis.text = element_text(face = "bold", size = 8),
      plot.title = element_text(face = "bold", size = 10)
    )
}


get_profile_sizes <- function(lpa_model) {
  # 1. Extract raw classification vector
  class_vector <- lpa_model$model$classification
  
  # 2. Build the dataframe explicitly from the vector
  data.frame(Profile = class_vector) %>%
    group_by(Profile) %>%
    summarise(N = n(), .groups = "drop") %>%
    mutate(Percentage = round((N / sum(N)) * 100, 2))
}


# ----------- LR Results ------------- #

render_full_regression_table <- function(model, caption = "Regression Results with Model Diagnostics") {
  
  # 1. Grab clean coefficients using broom::tidy
  raw_coefs=tidy(model)
  stat_label=if (inherits(model, "glm")) "z-value" else "t-value"
  p_label=if (inherits(model, "glm")) "Pr(>|z|)" else "Pr(>|t|)"
  
  # Format the main coefficient grid
  coef_rows=raw_coefs %>%
    mutate(
      estimate=as.character(round(estimate, 5)),
      std.error=as.character(round(std.error, 5)),
      statistic=as.character(round(statistic, 3)),
      p.value=ifelse(p.value < 0.001, "<2e-16 ***", as.character(round(p.value, 3)))
    ) %>%
    rename(
      Term=term,
      Estimate=estimate,
      `Std. Error`=std.error,
      !!stat_label:=statistic,
      !!p_label:=p.value
    )
  
  # 2. Extract fit statistics safely using broom::glance
  fit_stats=glance(model)
  
  # Handle missing observations string cleanly
  na_action=if (!is.null(model$na.action)) length(model$na.action) else 0
  missing_note=if (na_action > 0) paste0("  (", na_action, " observations deleted due to missingness)") else ""
  
  # Format F-statistic string
  f_stat_str=if ("statistic" %in% names(fit_stats) & "df" %in% names(fit_stats)) {
    paste0(round(fit_stats$statistic, 4), " on ", fit_stats$df - 1, " and ", fit_stats$df.residual, " DF")
  } else {
    "N/A"
  }
  
  f_p_val=if ("p.value" %in% names(fit_stats)) as.character(round(fit_stats$p.value, 4)) else "N/A"
  
  # 3. Create diagnostic summary rows using safe, static column labels
  diagnostic_rows=tibble(
    Term=c(
      "Residual standard error",
      "Multiple R-squared",
      "Adjusted R-squared",
      "F-statistic",
      "AIC",
      "BIC"
    ),
    Estimate=c(
      paste0(round(fit_stats$sigma, 3), " on ", fit_stats$df.residual, " degrees of freedom", missing_note),
      as.character(round(fit_stats$r.squared, 6)),
      as.character(round(fit_stats$adj.r.squared, 6)),
      f_stat_str,
      as.character(round(fit_stats$AIC, 2)),
      as.character(round(if("BIC" %in% names(fit_stats)) fit_stats$BIC else BIC(model), 2))
    ),
    `Std. Error`=c("", "", "", "p-value:", "", ""),
    TEMP_STAT_COL=c("", "", "", f_p_val, "", ""),
    TEMP_P_COL=c("", "", "", "", "", "")
  ) %>%
    # Dynamically match the column names to the coefficient table structure
    rename(
      !!stat_label:=TEMP_STAT_COL,
      !!p_label:=TEMP_P_COL
    )
  
  # 4. Bind rows together 
  final_table=bind_rows(coef_rows, diagnostic_rows)
  num_coefs=nrow(coef_rows)
  
  # 5. Render html table grid
  final_table %>%
    kable(
      format="html",
      caption=caption,
      align="lrrrr"
    ) %>%
    kable_styling(
      bootstrap_options=c("striped", "hover", "condensed"),
      full_width=TRUE,
      position="center"
    ) %>%
    # Bold the structural column header row
    row_spec(0, bold=TRUE, background="#F8F9FA") %>%
    # Add a solid dividing boundary separating the coefficients from the diagnostic rows
    row_spec(num_coefs, extra_css="border-bottom: 2px solid #dee2e6;")
}
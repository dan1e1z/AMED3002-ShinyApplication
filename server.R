# Server logic and helper functions for Kidney Disease Analysis Shiny App
library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggpubr)
library(plotly)
library(rsconnect)

# --------------------------------------------
# Data Loading and Cleaning Functions
# --------------------------------------------

load_and_clean_data <- function() {
  # Import data
  data1 <- read.csv("www/data/kidney_disease.csv")
  data2 <- read.csv("www/data/Chronic_Kidney_Disease.csv")
  
  # Data cleaning and preparation for data1
  colnames(data1) <- c('id', 'age', 'blood_pressure', 'specific_gravity', 'albumin', 'sugar', 'red_blood_cells', 'pus_cell',
                       'pus_cell_clumps', 'bacteria', 'blood_glucose_random', 'blood_urea', 'serum_creatinine', 'sodium',
                       'potassium', 'haemoglobin', 'packed_cell_volume', 'white_blood_cell_count', 'red_blood_cell_count',
                       'hypertension', 'diabetes_mellitus', 'coronary_artery_disease', 'appetite', 'peda_edema',
                       'anaemia', 'class')
  
  # Clean data
  data1$coronary_artery_disease <- trimws(data1$coronary_artery_disease)
  data1$hypertension <- trimws(data1$hypertension)
  data1$diabetes_mellitus <- trimws(data1$diabetes_mellitus)
  data1$coronary_artery_disease[data1$coronary_artery_disease == ""] <- NA
  data1$hypertension[data1$hypertension == ""] <- NA
  
  # Filter out rows with NA values
  data1 <- filter(data1, !is.na(coronary_artery_disease) & !is.na(serum_creatinine) & !is.na(albumin))
  
  data1$coronary_artery_disease_numerical <- ifelse(data1$coronary_artery_disease == "yes", 1, 0)
  
  # Data preparation for data2
  data2$BMI_category <- cut(data2$BMI,
                            breaks = c(-Inf, 18.5, 24.9, 29.9, Inf),
                            labels = c("Underweight", "Normal", "Overweight", "Obese"))
  
  # Convert the UrinaryTractInfections column into a factor with custom labels
  data2$UrinaryTractInfections_Status <- factor(data2$UrinaryTractInfections, labels = c("No Infection", "Yes Infection"))
  
  # Convert Smoking to factor for better plotting
  data2$Smoking_Status <- factor(data2$Smoking, levels = c(0, 1), labels = c("Non-Smoker", "Smoker"))
  
  return(list(data1 = data1, data2 = data2))
}

# --------------------------------------------
# Chi-Square Table Functions
# --------------------------------------------

create_chi_square_tables <- function(data1) {
  # Perform Chi-Square test for Hypertension vs Coronary Artery Disease
  hypertension_test <- chisq.test(table(data1$hypertension, data1$coronary_artery_disease))
  
  # Perform Chi-Square test for Diabetes vs Coronary Artery Disease
  diabetes_test <- chisq.test(table(data1$diabetes_mellitus, data1$coronary_artery_disease))
  
  dt1 <- data.frame(
    Statistic = c("Chi-square Statistic", "Degrees of Freedom", "P-value"),
    Value = c(round(hypertension_test$statistic, 3), 
              hypertension_test$parameter, 
              round(hypertension_test$p.value, 3))
  )
  
  dt2 <- data.frame(
    Statistic = c("Chi-square Statistic", "Degrees of Freedom", "P-value"),
    Value = c(round(diabetes_test$statistic, 3), 
              diabetes_test$parameter, 
              round(diabetes_test$p.value, 3))
  )
  
  list(
    hypertension = dt1,
    diabetes = dt2
  )
}

# --------------------------------------------
# Plotting Functions
# --------------------------------------------

# Function to create original graph (unadjusted) with color - now interactive
create_original_graph <- function(data1, plot_type = "box") {
  # Define color palette
  color_palette <- c("#4E79A7", "#F28E2B")
  
  if (plot_type == "logistic") {
    logit_model <- glm(coronary_artery_disease_numerical ~ serum_creatinine, data = data1, family = binomial)
    p_value <- summary(logit_model)$coefficients[2, 4]
    
    
    logit_plot <- ggplot(data1, aes(x = serum_creatinine, y = coronary_artery_disease_numerical, color = factor(coronary_artery_disease_numerical))) + 
      geom_point(alpha = 0.6, size = 2) +
      stat_smooth(method = "glm", se = FALSE, method.args = list(family = binomial()), color = "#780f51", size = 1.2) +
      scale_color_manual(values = c("#e76518", "#20446e")) + 
      scale_y_continuous(breaks = c(0, 1), labels = c("No CAD", "Yes CAD")) +
      labs(
        x = "Serum Creatinine (mg/dL)",
        y = "Probability of Coronary Artery Disease"
      ) +
      # Adjust y-values to be within 0-1 range
      annotate("text", x = max(data1$serum_creatinine) * 0.8, y = 0.8, 
               label = paste("p =", format(p_value, digits = 3)), 
               color = "black", size = 5) +
      annotate("text", x = max(data1$serum_creatinine) * 0.8, y = 0.9, 
               label = paste("y =", round(coef(logit_model)[1], 3), 
                             "+", round(coef(logit_model)[2], 3), "x"), 
               color = "black", size = 5) +
      coord_cartesian(clip = "off") +
      theme_minimal() +
      theme(legend.position = "none")
    
    return(logit_plot)
    
  }
  
  # Create base plot
  base_plot <- ggplot(data1, aes(x = coronary_artery_disease, y = serum_creatinine, 
                                 fill = coronary_artery_disease, color = coronary_artery_disease)) +
    labs(title = "Unadjusted Serum Creatinine vs Coronary Artery Disease",
         x = "Coronary Artery Disease Status", 
         y = "Serum Creatinine (mgs/dl)") +
    scale_fill_manual(values = color_palette) +
    scale_color_manual(values = c("#2E4057", "#D16103")) +
    theme_minimal() +
    theme(legend.position = "none")
  
  # Add plot elements based on type
  if (plot_type == "violin") {
    final_plot <- base_plot + 
      geom_violin(trim = FALSE, alpha = 0.7) + 
      geom_boxplot(width = 0.1, fill = "white", alpha = 0.7)
  } else { # Default to boxplot
    final_plot <- base_plot + 
      geom_boxplot(alpha = 0.8)
  }
  
  # Add statistical comparison
  final_plot <- final_plot + ggpubr::stat_compare_means(label.y = max(data1$serum_creatinine, na.rm = TRUE) * 1.1 )
  
  # Convert to interactive plot
  return(final_plot)
}

# Function to create the adjusted graph or logistic regression
create_adjusted_graph <- function(data1, remove_hypertension = FALSE, remove_diabetes = FALSE, plot_type = "box") {
  plot_data <- data1  # Copy data for modifications
  
  # Define color palette
  color_palette <- c("#4E79A7", "#F28E2B")
  
  # Build formula based on selected confounders
  confounders <- c()
  if (remove_hypertension) confounders <- c(confounders, "hypertension")
  if (remove_diabetes) confounders <- c(confounders, "diabetes_mellitus")
  
  # Adjust data if confounders are selected
  if (length(confounders) > 0) {
    formula <- as.formula(paste("serum_creatinine ~", paste(confounders, collapse = " + ")))
    model <- lm(formula, data = plot_data)
    plot_data$serum_creatinine_adjusted <- residuals(model)
    adj_text <- paste("Adjusted for", paste(confounders, collapse = " and "))
  } else {
    plot_data$serum_creatinine_adjusted <- plot_data$serum_creatinine
    adj_text <- "Unadjusted"
  }
  
  # Check if we need to run a logistic regression
  if (plot_type == "logistic") {
    logit_formula <- as.formula(paste("coronary_artery_disease_numerical ~ serum_creatinine_adjusted", 
                                      if (length(confounders) > 0) paste("+", paste(confounders, collapse = " + ")) else ""))
    logit_model <- glm(logit_formula, data = plot_data, family = binomial)
    p_value <- summary(logit_model)$coefficients[2, 4]
    
    logit_plot <- ggplot(plot_data, aes(x = serum_creatinine_adjusted, y = coronary_artery_disease_numerical)) + 
      geom_point(aes(colour = factor(coronary_artery_disease_numerical)), alpha = 0.6, size = 2) + 
      stat_smooth(method = "glm", se = FALSE, method.args = list(family = binomial()), colour = "#780f51", size = 1.2) +  
      scale_color_manual(values = c("#e76518", "#20446e")) + 
      scale_y_continuous(breaks = c(0, 1), labels = c("No CAD", "Yes CAD")) + 
      labs(
        x = "Adjusted Serum Creatinine (mg/dL)",
        y = "Probability of Coronary Artery Disease"
      ) +
      # Adjust y-values to be within 0-1 range
      annotate("text", x = max(data1$serum_creatinine) * 0.8, y = 0.8, 
               label = paste("p =", format(p_value, digits = 3)), 
               color = "black", size = 5) +
      annotate("text", x = max(data1$serum_creatinine) * 0.8, y = 0.9, 
               label = paste("y =", round(coef(logit_model)[1], 3), 
                             "+", round(coef(logit_model)[2], 3), "x"), 
               color = "black", size = 5) +
      coord_cartesian(clip = "off") +
      theme_minimal() +
      theme(legend.position = "none") 
    
    
    return(logit_plot)
  }
  
  # Create base plot (only for "box" or "violin")
  base_plot <- ggplot(plot_data, aes(x = coronary_artery_disease, y = serum_creatinine_adjusted, 
                                     fill = coronary_artery_disease, color = coronary_artery_disease)) +
    labs(
      title = paste("Serum Creatinine", adj_text, "vs Coronary Artery Disease"),
      x = "Coronary Artery Disease Status",
      y = paste("Serum Creatinine (mgs/dl)", adj_text)
    ) +
    scale_fill_manual(values = color_palette) +
    scale_color_manual(values = c("#2E4057", "#D16103")) +
    theme_minimal() +
    theme(legend.position = "none")
  
  # Add plot elements based on type
  if (plot_type == "violin") {
    final_plot <- base_plot + 
      geom_violin(trim = FALSE, alpha = 0.7) + 
      geom_boxplot(width = 0.1, fill = "white", alpha = 0.7)
  } else {  # Default to boxplot
    final_plot <- base_plot + 
      geom_boxplot(alpha = 0.8)
  }
  
  # Add statistical comparison
  final_plot <- final_plot + ggpubr::stat_compare_means(label.y = max(plot_data$serum_creatinine_adjusted, na.rm = TRUE) * 1.1 )
  
  # Convert to interactive plot
  return(final_plot)
}

# Function to create BMI vs Serum Creatinine plot
create_bmi_creatinine_smoking_plot <- function(data2) {
  final_plot <- ggplot(data2, aes(x = BMI_category, y = SerumCreatinine, fill = Smoking_Status)) +
    geom_boxplot() +
    labs(
      x = "BMI Category",
      y = "Serum Creatinine (mgs/dl)",
      fill = "Smoking Status"
    ) +
    theme_minimal()
  # Add statistical comparison
  final_plot <- final_plot + ggpubr::stat_compare_means(label.y = max(data2$SerumCreatinine, na.rm = TRUE) * 1.1)
  
  # Convert to interactive plot
  plotly::ggplotly(final_plot)
}

create_explore_relationship_graph <- function(data1) {
  condition_cols <- c(
    "hypertension",
    "diabetes_mellitus",
    "coronary_artery_disease",
    "appetite",
    "peda_edema",
    "anaemia"
  )
  
  # Find available conditions
  available_conditions <- condition_cols[condition_cols %in% names(data1)]
  
  # Create plot data
  df_long <- data1 %>%
    pivot_longer(cols = all_of(available_conditions),
                 names_to = "condition",
                 values_to = "status") %>%
    mutate(
      condition = factor(condition,
                         levels = condition_cols,
                         labels = c("Hypertension", "Diabetes Mellitus",
                                    "Coronary Artery Disease", "Appetite",
                                    "Pedal Edema", "Anemia")),
      status = as.factor(status)
    )
  
  # Create plot
  ggplot(df_long, aes(x = serum_creatinine, fill = status)) +
    geom_density(alpha = 0.7, color = NA) +
    facet_wrap(~ condition, ncol = 2) +
    labs(x = "Serum Creatinine (mgs/dl)",
         y = "Density",
         title = "Distribution of Serum Creatinine by Health Conditions",
         fill = "Condition Status",
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      strip.text = element_text(face = "bold", size = 10),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.title = element_text(face = "bold"),
      legend.title = element_text(face = "bold"),
    ) +
    scale_fill_brewer(palette = "Set2") +
    scale_x_log10()  # Often useful for clinical values 
}

# --------------------------------------------
# Statistical Test Functions
# --------------------------------------------

# Function to perform statistical tests and create formatted results tables
perform_statistical_tests <- function(data2) {
  # T-test for Serum Creatinine by smoking status
  smoking_ttest <- t.test(SerumCreatinine ~ Smoking, data = data2)
  
  # Format t-test results
  smoking_results <- data.frame(
    Statistic = c("t-statistic", "Degrees of Freedom", "p-value", "Mean in Non-Smokers", "Mean in Smokers"),
    Value = c(
      round(smoking_ttest$statistic, 3),
      round(smoking_ttest$parameter, 3),
      round(smoking_ttest$p.value, 3),
      round(smoking_ttest$estimate[1], 3),
      round(smoking_ttest$estimate[2], 3)
    )
  )
  
  # One-way ANOVA for BMI categories
  bmi_anova <- aov(SerumCreatinine ~ BMI_category, data = data2)
  anova_summary <- summary(bmi_anova)
  
  # Format ANOVA results
  anova_results <- data.frame(
    Statistic = c("F-statistic", "p-value"),
    Value = c(
      round(anova_summary[[1]][1, 4], 3),
      round(anova_summary[[1]][1, 5], 3)
    )
  )
  
  # Return all results
  return(list(
    smoking_results = smoking_results,
    bmi_anova = anova_results
  ))
}

# --------------------------------------------
# Server Function
# --------------------------------------------

# --------------------------------------------
# Server Function
# --------------------------------------------
server <- function(input, output, session) {
  # Load and prepare data
  data_list <- load_and_clean_data()
  data1 <- data_list$data1
  data2 <- data_list$data2
  
  # Create chi-square tables
  chi_tables <- create_chi_square_tables(data1)
  
  # Perform statistical tests
  stat_tests <- perform_statistical_tests(data2)
  
  # Dataset 1: Hypertension and Diabetes Tables
  output$hypertension_table <- renderTable({
    chi_tables$hypertension
  })
  
  output$diabetes_table <- renderTable({
    chi_tables$diabetes
  })
  
  # Dataset 1: Unadjusted and Adjusted Graphs
  output$original_cofounder_graph <- renderPlotly({
    create_original_graph(data1, input$graph_type)
  })
  
  output$adjusted_cofounder_graph <- renderPlotly({
    create_adjusted_graph(data1, 
                          remove_hypertension = input$remove_hypertension, 
                          remove_diabetes = input$remove_diabetes, 
                          plot_type = input$graph_type)
  })
  
  filtered_data <- reactive({
    filter(data1, 
           (input$creatinine_severity_filter == "all" | 
              (input$creatinine_severity_filter == "normal" & serum_creatinine <= 1.2) |
              (input$creatinine_severity_filter == "moderate" & serum_creatinine > 1.2 & serum_creatinine <= 2.5) |
              (input$creatinine_severity_filter == "severe" & serum_creatinine > 2.5)))
  })
  
  filtered_data_age <- reactive({
    filter(filtered_data(), 
           age >= input$age_group[1] & age <= input$age_group[2])
  })
  
  output$explore_relationship_graph <- renderPlotly({
    create_explore_relationship_graph(filtered_data_age())
  })
  
  # Dataset 2: Serum Creatinine vs Smoking and BMI Analysis
  output$bmi_creatinine_smoking_plot <- renderPlotly({
    create_bmi_creatinine_smoking_plot(data2)
  })
  
  # Dataset 2: Statistical Test Results
  output$smoking_ttest_table <- renderTable({
    stat_tests$smoking_results
  })
  
  output$bmi_anova_table <- renderTable({
    stat_tests$bmi_anova
  })
  
  output$introductionImage <- renderImage({ 
    filename <- normalizePath(file.path('./www/images','heart.jpg'))
    
    list(src = filename,
         contentType = 'image/png',
         alt = "This is alternate text")
  }, deleteFile = FALSE)
}
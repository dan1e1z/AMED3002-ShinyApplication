# UI definition for Kidney Disease Analysis Shiny App

# Define UI
ui <- fluidPage(
  theme = shinytheme("cosmo"),
  titlePanel("Kidney Disease Data Exploration"),
  
  tabsetPanel(
    
    # Introduction Panel
    tabPanel("Introduction",
             h2("Introduction"),
             column(6,
                    h2("Introduction"),  
                    
                    p("This Shiny app explores the relationship between kidney function (serum creatinine) and coronary artery disease (CAD),  
   examining key risk factors like hypertension, diabetes, BMI, and smoking. Through statistical analysis and visualizations,  
   it provides insights into cardiorenal syndrome, aiding medical students, researchers, and healthcare professionals."),  
                    
                    h3("Research Question"),  
                    p("How does serum creatinine relate to CAD, and what role do hypertension, diabetes, BMI, and smoking play?"),  
                    
                    h3("Key Objectives"),  
                    tags$ul(
                      tags$li("Identify confounders like hypertension and diabetes."),  
                      tags$li("Analyze serum creatinine in relation to CAD and key risk factors."),  
                      tags$li("Use statistical tests (Chi-Square, Logistic Regression, ANOVA) to support findings."),  
                      tags$li("Provide interactive data visualizations.")  
                    ) 
                    
             ),
             column(6, 
                    imageOutput("introductionImage"),
                    p("Fakurian, M. (2023). A heart-shaped glass sitting on top of a piece of wood [Photograph]. Unsplash. https://unsplash.com/photos/iG3PgPB7568")
             ),
    ),
    
    # Dataset 1 Analysis Panel
    tabPanel("Dataset 1 Results",
             h2("Dataset 1 Results"),
             h3("Exploring Data Relationship"),
             fluidRow(
               column(2, 
                      wellPanel(
                        h4("Filter Options"),
                        radioButtons("creatinine_severity_filter", 
                                     label = "Serum Creatinine Severity Filter", 
                                     choices = list("All" = "all", 
                                                    "Normal Range (≤ 1.2 mg/dL)" = "normal", 
                                                    "Moderate Elevation (1.2-2.5 mg/dL)" = "moderate", 
                                                    "Severe Elevation (> 2.5 mg/dL)" = "severe"), 
                                     selected = "all"),
                        sliderInput("age_group", 
                                    label = "Select Age Group:", 
                                    min = 0, 
                                    max = 120, 
                                    value = c(20, 60), 
                                    step = 1, 
                                    animate = TRUE)
                        
                      )
               ),
               column(10,
                      plotlyOutput("explore_relationship_graph"),
                      br(),
                      p(
                        "Figiure 1. Density plots of serum creatinine levels (mg/dL) stratified by health conditions. 
                        Each facet represents a specific condition, with distributions coloured by condition status.
                        The x-axis is log-transformed for better visualisation of clinical values."
                      )
               )
             ),
             h3("Identifying Confounders"),
             fluidRow(
               column(6, 
                      p("This section explores potential confounders such as hypertension and diabetes, and their relationship with coronary artery disease."),
                      br(),
                      br(),
                      p(
                        "Table 1. Chi-Square Test Results for Associations Between Health Conditions."
                      ),
                      p(
                        "The table includes the chi-square statistic, degrees of freedom, and p-value for the relationships between 
                        (1) hypertension and coronary artery disease and (2) diabetes mellitus and coronary artery disease"
                      ),
               ),
               column(3, 
                      h4("(1) Hypertension vs Coronary Artery Disease"),
                      tableOutput("hypertension_table"),
               ),
               column(3, 
                      h4("(2) Diabetes vs Coronary Artery Disease"),
                      tableOutput("diabetes_table")
               )
             ),
             h3("Effect of Removing Confounders"),
             fluidRow(
               column(12,
                      wellPanel(
                        h4("Filter Options"),
                        checkboxInput("remove_hypertension", "Remove Hypertension", FALSE),
                        checkboxInput("remove_diabetes", "Remove Diabetes", FALSE),
                        selectInput("graph_type", "Graph Type",
                                    choices = c("Violin Plot" = "violin", 
                                                "Box Plot" = "box",
                                                "Logistic Regression" = "logistic"))
                      )
               ),
               fluidRow(12,
                        column(6, 
                               plotlyOutput("original_cofounder_graph")
                        ),
                        column(6, 
                               plotlyOutput("adjusted_cofounder_graph")
                        ),
               ),
               br(),
               p("Figure 2. Association between serum creatinine levels (mg/dL) and coronary artery disease status with adjustment for potential confounders (hypertension, diabetes). 
                 Visualization options include distribution plots (boxplot, violin) and logistic regression with corresponding statistical outputs (p-values, regression coefficients)."
               ),
             )
    ),
    
    # Dataset 2 Analysis Panel
    tabPanel("Dataset 2 Results",
             h2("Dataset 2 Results"),
             fluidRow(
               column(6, 
                      p("This section explores predictivity of Serum Creatinine against smoking and BMI, both known risk factors of CAD"),
                      br(),
                      br(),
                      p("Table 2. Analysis of Serum Creatinine Differences Across Smoking Groups and BMI Categories"),
                      p(
                        "Results of statistical analyses comparing serum creatinine levels across smoking status and BMI categories. 
                       (1) Independent samples t-test comparing smokers vs. non-smokers. 
                       (2) One-way ANOVA comparing across BMI categories."
                      ),
               ),
               column(3, 
                      h4("(1) Statistical Tests for Smoking and Serum Creatinine"),
                      tableOutput("smoking_ttest_table"),
               ),
               column(3, 
                      h4("(2) Statistical Tests for BMI Categories and Serum Creatinine"),
                      tableOutput("bmi_anova_table"),
               ),
             ),
             h3("Serum Creatinine vs BMI and Smoking"),
             plotlyOutput("bmi_creatinine_smoking_plot"),
             br(),
             p(
               "Figure 3. Association between serum creatinine levels (mg/dL) and BMI categories, stratified by smoking status. 
               Boxplots display median values with interquartile ranges. Statistical comparisons between BMI categories are annotated with p-values."
             )
    ),
    
    tabPanel("Analysis & Conclusion",
             h2("Analysis"),
             
             h3("Dataset 1 Analysis: Coronary Artery Disease & Serum Creatinine"),  
             
             tags$h4("Chi-Square Analysis (Table 1)"),  
             p("The chi-square test indicates a strong relationship between hypertension and CAD (p = 0.00, Table 1 (1)),  
                as well as diabetes and CAD (p = 0.00, Table 1 (2)).  
                Since both hypertension and diabetes are known contributors to kidney dysfunction,  
                this supports the link between CAD and impaired kidney function."),  
             br(),  
             
             tags$h4("Boxplot Analysis (Figure 2)"),  
             p("Boxplot analysis compares serum creatinine levels across groups:"),
             tags$ul(
               tags$li("When both hypertension and diabetes are present, creatinine levels significantly differ (p = 8e-08)."),  
               tags$li("Removing hypertension (p = 0.13) or both conditions (p = 0.18) eliminates statistical significance,  
                       indicating hypertension may be the dominant factor affecting creatinine levels."),  
               tags$li("Removing only diabetes (p = 0.014) reduces the effect size but maintains significance,  
                       showing diabetes also contributes to kidney dysfunction.")  
             ),  
             br(),  
             
             tags$h4("Logistic Regression Analysis (Figure 2)"),  
             p("Logistic regression further supports the connection between CAD and serum creatinine:"),
             tags$ul(
               tags$li("With all factors included, higher serum creatinine is significantly associated with increased CAD risk (p = 0.0102)."),  
               tags$li("When hypertension or diabetes are removed, the relationship between creatinine and CAD becomes non-significant,  
                       suggesting these conditions mediate the link between kidney function and CAD.")
             ),  
             br(),  
             
             h3("Dataset 2 Analysis: BMI, Smoking & Serum Creatinine"),  
             
             tags$h4("T-test: Smoking & Serum Creatinine (Table 2 (1))"),  
             p("The t-test finds no significant difference in serum creatinine levels between smokers and non-smokers (p = 0.38)."),  
             br(),  
             
             tags$h4("ANOVA: BMI & Serum Creatinine (Table 2 (2))"),  
             p("ANOVA results suggest that BMI categories may influence serum creatinine,  
                though results are close to significance (p = 0.06)."),  
             br(),  
             
             tags$h4("Boxplot & Kruskal-Wallis Test (Figure 3)"),  
             p("Breaking down BMI groups further reveals:"),
             tags$ul(
               tags$li("Smoking significantly affects serum creatinine in underweight individuals (p = 0.018) and obese individuals (p = 0.043)."),  
               tags$li("No significant differences are observed in normal-weight (p = 0.8) or overweight groups (p = 0.49)."),  
               tags$li("This suggests that smoking's effect on kidney function may be influenced by extreme BMI levels.")  
             ),  
             br(),  
             
             h2("Conclusion"),  
             tags$b("Key Takeaways"),  
             tags$ul(
               tags$li("Serum creatinine is strongly linked to CAD when hypertension and diabetes are present,  
                       reinforcing the kidney-cardiovascular connection (Figure 2, Table 1)."),  
               tags$li("Hypertension plays a major role in influencing serum creatinine and CAD risk,  
                       while diabetes also contributes (Table 1)."),  
               tags$li("Smoking alone does not significantly impact serum creatinine,  
                       but in underweight and obese individuals, smoking may contribute to kidney dysfunction (Figure 3)."),  
               tags$li("BMI's effect on kidney function remains uncertain but could be relevant in extreme weight groups (Table 2).")  
             ),  
             br(),  
             p("These findings support the study of cardiorenal syndrome, highlighting the need for monitoring at-risk individuals  
                for both cardiovascular and kidney-related complications.")
    )
  )
)
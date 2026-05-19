library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(haven)

# load & filter data
data_main <- readRDS("data_coded_full_grace.rds") %>% filter(is.na(study))

# replace placeholder in questions
clean_q_label <- function(lbl) gsub("\\[Field-program_info\\]", "these programs", lbl)

# age buckets
age_levels <- c("18-24", "25-34", "35-44", "45-54", "55-64", "65+")

# clean and recode demographic variables for sidebar filters
cleaned <- data_main %>%
  mutate(
    # collapse income into 5 groups
    income_num = as.numeric(p_income),
    income_5 = case_when(
      income_num %in% c(1, 2) ~ "Less than $20,000",
      income_num %in% c(3, 4) ~ "$20,001 - $40,000",
      income_num %in% c(5, 6) ~ "$40,001 - $80,000",
      income_num %in% c(7, 8) ~ "$80,001 - $140,000",
      income_num %in% c(9, 10) ~ "More than $140,001",
      TRUE ~ NA_character_
    ),
    
    # collapse education into 3 groups
    educ_num = as.numeric(p_own_educ),
    educ_3 = case_when(
      educ_num %in% c(1,2,3) ~ "HS or less / some college",
      educ_num %in% c(4,5) ~ "2-year / 4-year degree",
      educ_num %in% c(6,7) ~ "Master's / doctorate",
      TRUE ~ NA_character_
    ),
    
    ideo_lbl = as.character(as_factor(p_ideology)),
    gender_lbl = as.character(as_factor(p_gender)),
    
    age_num = as.numeric(p_age),
    age_bin = case_when(
      age_num <= 24 ~ "18-24",
      age_num <= 34 ~ "25-34",
      age_num <= 44 ~ "35-44",
      age_num <= 54 ~ "45-54",
      age_num <= 64 ~ "55-64",
      age_num < 120 ~ "65+",
    ),
    
    white = (p_race_white == 1) | (p_race_1 == 1), 
    black = (p_race_black == 1) | (p_race_3 == 1), 
    asian = (p_race_asian == 1) | (p_race_5 == 1), 
    hispanic = (p_race_hispanic == 1) | (p_ethnicity == 1), 
    native = (p_race_native == 1) | (p_race_4 == 1), 
    mena = (p_race_mena == 1) | (p_race_6 == 1), 
    other = (p_race_other == 1) | (p_race_7 == 1),
    
    race_sum = white + black + asian + hispanic + native + mena + other,
    
    race_collapsed = case_when( 
      race_sum > 1 ~ "Multiracial", 
      white ~ "White", 
      black ~ "Black", 
      asian ~ "Asian", 
      hispanic ~ "Hispanic/Latino", 
      native ~ "Native/Indigenous", 
      mena ~ "Middle Eastern/N. African", 
      other ~ "Other"
    ),
    
    welfare = case_when( 
      p_welfare_1 == 1 & p_welfare_2 == 1 ~ "Self + Someone close", 
      p_welfare_1 == 1 ~ "Self", 
      p_welfare_2 == 1 ~ "Someone close", 
      p_welfare_0 == 1 ~ "No", 
    )
  )

# sidebar var choices
income_choices <- c(
  "Less than $20,000",
  "$20,001 - $40,000",
  "$40,001 - $80,000",
  "$80,001 - $140,000",
  "More than $140,001"
)
edu_choices <- c(
  "HS or less / some college",
  "2-year / 4-year degree",
  "Master's / doctorate"
)
ideo_choices <- levels(as_factor(data_main$p_ideology))
race_choices <- sort(unique(na.omit(cleaned$race_collapsed)))
gender_choices <- sort(unique(na.omit(cleaned$gender_lbl)))
welfare_choices <- sort(unique(na.omit(cleaned$welfare)))

# eligible questions
eligible_vars <- names(data_main)[vapply(names(data_main), function(v) {
  x <- data_main[[v]]
  if (startsWith(v, "p_")) return(FALSE)
  if (!inherits(x, "haven_labelled")) return(FALSE)
  xf <- as_factor(x)
  xf <- xf[!is.na(xf)]
  length(unique(xf)) >= 3
}, logical(1))]

# reformat question to include variables
question_choices <- setNames(eligible_vars,
                             paste0("[", eligible_vars, "] ", sapply(eligible_vars, function(v) attr(data_main[[v]], "label"))) 
)

codebook_link <- "https://drive.google.com/file/d/1t43JnC16S7mvpSZZ9-gxn0RcVefJiM01/view?usp=sharing"


# UI
ui <- page_sidebar(
  title = "Explorer",
  sidebar = sidebar(
    width = 300,
    
    selectInput("question_x", "Select X question:", choices = question_choices),
    selectInput("question_y", "Select Y question:", choices = question_choices),
    
    selectizeInput("income", "Household income", choices = income_choices, multiple = TRUE),
    selectizeInput("educ", "Education", choices = edu_choices, multiple = TRUE),
    selectizeInput("ideo", "Political ideology", choices = ideo_choices, multiple = TRUE),
    selectizeInput("race", "Race", choices = race_choices, multiple = TRUE),
    selectizeInput("gender", "Gender", choices = gender_choices, multiple = TRUE),
    selectizeInput("welfare_sel", "Welfare experience", choices = welfare_choices, multiple = TRUE),
    selectizeInput("age", "Age", choices = age_levels, multiple = TRUE),
    
    radioButtons("mode", "Display distribution as:",
                 choices = c("Percent" = "pct", "Count" = "n"),
                 selected = "pct"
    ),
    
    hr(),
    a("Codebook", href = codebook_link, target = "_blank")
  ),
  
  tabsetPanel(
    tabPanel("Distribution", plotOutput("dist_plot")),
    tabPanel("Mean", tableOutput("mean_table")),
    tabPanel("Scatter", plotOutput("scatter_plot"))
  )
)

# SERVER
server <- function(input, output) {
  filtered <- reactive({
    d <- cleaned
    if (length(input$income)) d <- d %>% filter(.data$income_5 %in% input$income)
    if (length(input$educ)) d <- d %>% filter(.data$educ_3 %in% input$educ)
    if (length(input$ideo)) d <- d %>% filter(.data$ideo_lbl %in% input$ideo)
    if (length(input$race)) d <- d %>% filter(.data$race_collapsed %in% input$race)
    if (length(input$gender)) d <- d %>% filter(.data$gender_lbl %in% input$gender)
    if (length(input$welfare_sel)) d <- d %>% filter(.data$welfare %in% input$welfare_sel)
    if (length(input$age)) d <- d %>% filter(.data$age_bin %in% input$age)
    d
  })
  
  # MEAN TABLE
  output$mean_table <- renderTable({
    d <- filtered()
    y <- as.numeric(d[[input$question_x]])
    data.frame(
      N = sum(!is.na(y)),
      Mean = round(mean(y), 2),
      SD = round(sd(y), 2),
      Median = round(median(y), 2)
    )
  })
  
  # HISTOGRAM PLOT
  output$dist_plot <- renderPlot({
    d <- filtered()
    q_fac <- as.character(as_factor(d[[input$question_x]]))
    
    df <- data.frame(resp = q_fac) %>%
      count(resp, name = "n") %>%
      mutate(pct = 100 * n / sum(n))
    
    ycol <- if (input$mode == "pct") "pct" else "n"
    ylab <- if (input$mode == "pct") "Percent" else "Count"
    # extract original value labels from the full dataset
    x_var <- data_main[[input$question_x]]
    vl <- attr(x_var, "labels")
    # get label names and numeric codes
    labs <- names(vl)
    codes <- as.numeric(vl)
    # order the bars
    df$resp <- factor(df$resp, levels = labs)
    label_map <- setNames(paste0(codes, " = ", labs), labs)
    
    ggplot(df, aes(x = resp, y = .data[[ycol]])) +
      geom_col() +
      coord_flip() +
      scale_x_discrete(labels = label_map) +
      labs(x = NULL, y = ylab, caption = paste0("N = ", sum(df$n))) +
      theme_minimal(base_size = 12)
  }, res = 100, height = 500)
  
  
  # SCATTER PLOT
  output$scatter_plot <- renderPlot({
    d <- filtered()
    N <- sum(complete.cases(d[[input$question_x]], d[[input$question_y]]))
    # get original labeled variables (for axis labels)
    x_var <- data_main[[input$question_x]]
    y_var <- data_main[[input$question_y]]
    # extract numeric codes and labels
    x_vl <- attr(x_var,"labels")
    y_vl <- attr(y_var,"labels")
    x_map <- setNames(paste0(x_vl," = ",names(x_vl)),as.character(x_vl))
    y_map <- setNames(paste0(y_vl," = ",names(y_vl)),as.character(y_vl))
    
    ggplot(d, aes(
      x = as.numeric(.data[[input$question_x]]),
      y = as.numeric(.data[[input$question_y]])
    )) +
      geom_jitter(width = 0.15, height = 0.15, alpha = 0.25) +
      geom_smooth(method = "lm") +
      scale_x_continuous(
        breaks = as.numeric(x_vl),
        labels = function(x) unname(x_map[as.character(x)])
      ) +
      scale_y_continuous(
        breaks = as.numeric(y_vl),
        labels = function(x) unname(y_map[as.character(x)])
      ) +
      labs(x = input$question_x, y = input$question_y, caption = paste0("N = ", N)) +
      theme_minimal(base_size = 12)
  })
}

shinyApp(ui, server)


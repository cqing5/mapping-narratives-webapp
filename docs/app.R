library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(haven)

# load & filter data
data_main <- readRDS("data_coded_full.rds") %>% filter(is.na(study))

# replace placeholder in questions
clean_q_label <- function(lbl) {gsub("\\[Field-program_info\\]", "these programs", lbl)}

# clean and recode demographic variables
cleaned <- data_main %>%
  mutate(
    income_num = as.numeric(p_income),
    income_5 = case_when(
      income_num %in% c(1, 2) ~ "Less than $20,000",
      income_num %in% c(3, 4) ~ "$20,001 - $40,000",
      income_num %in% c(5, 6) ~ "$40,001 - $80,000",
      income_num %in% c(7, 8) ~ "$80,001 - $140,000",
      income_num %in% c(9, 10) ~ "More than $140,001",
      TRUE ~ NA_character_
    ),
    
    educ_num = as.numeric(p_own_educ),
    educ_4 = case_when(
      educ_num %in% c(1, 2) ~ "HS or less",
      educ_num %in% c(3, 4) ~ "Some college / 2-year college degree",
      educ_num == 5 ~ "4-year college degree",
      educ_num %in% c(6, 7) ~ "Master's / Doctorate",
      TRUE ~ NA_character_
    ),
    
    ideo_lbl = as.character(as_factor(p_ideology)),
    gender_lbl = as.character(as_factor(p_gender)),
    
    age_num = as.numeric(p_age),
    age_6 = case_when(
      age_num <= 24 ~ "18-24",
      age_num <= 34 ~ "25-34",
      age_num <= 44 ~ "35-44",
      age_num <= 54 ~ "45-54",
      age_num <= 64 ~ "55-64",
      age_num < 120 ~ "65+",
      TRUE ~ NA_character_
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
      other ~ "Other",
      TRUE ~ NA_character_
    ),
    
    welfare = case_when(
      p_welfare_1 == 1 & p_welfare_2 == 1 ~ "Self + Someone close",
      p_welfare_1 == 1 ~ "Self",
      p_welfare_2 == 1 ~ "Someone close",
      p_welfare_0 == 1 ~ "No",
      TRUE ~ NA_character_
    )
  )

# fixed category orders
income_choices <- c("Less than $20,000", "$20,001 - $40,000", "$40,001 - $80,000",
                    "$80,001 - $140,000", "More than $140,001")
ideo_choices <- c("Very Conservative", "Conservative", "Moderate", "Liberal",
                  "Very Liberal")
age_choices <- c("18-24", "25-34", "35-44", "45-54", "55-64", "65+")
edu_choices <- c("HS or less", "Some college / 2-year college degree",
                 "4-year college degree", "Master's / Doctorate")

# maps variables to their category order 
order_map <- list(
  income_5 = income_choices,
  educ_4 = edu_choices,
  ideo_lbl = ideo_choices,
  age_6 = age_choices
)

# for UI
group_choices <- c(
  "Household income" = "income_5",
  "Education" = "educ_4",
  "Political ideology" = "ideo_lbl",
  "Race" = "race_collapsed",
  "Gender" = "gender_lbl",
  "Welfare experience" = "welfare",
  "Age" = "age_6"
)

# helper functions
# check ordering for the demo category
apply_order <- function(x, var_name) {
  if (is.null(order_map[[var_name]])) {
    factor(x)
  } else {
    factor(x, levels = order_map[[var_name]])
  }
}

# convert variable name to UI label
get_group_label <- function(group_var) {
  names(group_choices)[group_choices == group_var]
}

# convert counts to percent 
add_plot_value <- function(df, mode) {
  df$value <- if (mode == "pct") 100 * df$n / sum(df$n) else df$n
  list(df = df, ylab = if (mode == "pct") "Percent" else "Count")
}

# order response options
get_response_labels <- function(question_var) { 
  vl <- attr(data_main[[question_var]], "labels") 
  lbls <- names(vl) 
  codes <- as.numeric(vl) 
  list(levels = lbls, labels = setNames(paste0(codes, " = ", lbls), lbls)) 
}

# calculate summary stats
summarise_response <- function(df) {
  df %>%
    summarise(
      N = sum(!is.na(response)),
      Mean = round(mean(response, na.rm = TRUE), 2),
      SD = round(sd(response, na.rm = TRUE), 2),
      Median = round(median(response, na.rm = TRUE), 2),
    )
}

# eligible questions
eligible_vars <- names(data_main)[vapply(names(data_main), function(v) {
  x <- data_main[[v]]
  if (startsWith(v, "p_")) return(FALSE)
  if (!inherits(x, "haven_labelled")) return(FALSE)
  xf <- as_factor(x)
  xf <- xf[!is.na(xf)]
  length(unique(xf)) >= 3
}, logical(1))]

question_choices <- setNames(
  eligible_vars,
  paste0("[", eligible_vars, "] ", 
         sapply(eligible_vars, function(v) clean_q_label(attr(data_main[[v]], "label"))))
)

codebook_link <- "https://drive.google.com/file/d/1t43JnC16S7mvpSZZ9-gxn0RcVefJiM01/view?usp=sharing"


# UI
ui <- navbarPage(
  "RSF Mapping Narratives Data Explorer",
  
  tabPanel("Overall Distribution",
           sidebarLayout(sidebarPanel(
             width = 3,
             selectInput("hist_question", "Select question:", choices = question_choices),
             radioButtons("hist_mode", "Display distribution as:",
                          choices = c("Percent" = "pct", "Count" = "n"), selected = "pct"),
             hr(),
             a("Codebook", href = codebook_link, target = "_blank")),
             mainPanel(
               h4(textOutput("hist_title")),
               plotOutput("hist_plot", height = 600),
               br(),
               h4("Means Table"),
               tableOutput("hist_mean_table"),
               p(tags$i("This shows the distribution of responses across the entire sample. To see how responses vary by sociodemographic characteristics, use the 'Stacked Bar Charts' tab."))
             )
           )
  ),
  
  tabPanel("Stacked Bar Charts",
           sidebarLayout(sidebarPanel(
             width = 3,
             selectInput("sbc_question", "Select question:", choices = question_choices),
             radioButtons("sbc_group", "Split bars by:", choices = group_choices),
             radioButtons("sbc_mode", "Display distribution as:",
                          choices = c("Percent" = "pct", "Count" = "n"), selected = "pct"),
             hr(),
             a("Codebook", href = codebook_link, target = "_blank")
           ),
           mainPanel(
             h4(textOutput("sbc_title")),
             plotOutput("sbc_plot", height = 600),
             br(),
             h4("Means Table"),
             tableOutput("sbc_mean_table")
           )
           )
  ),
  
  tabPanel("Scatter Plot",
           sidebarLayout(sidebarPanel(
             width = 3,
             selectInput("scatter_question_x", "Select X question:", choices = question_choices),
             selectInput("scatter_question_y", "Select Y question:", choices = question_choices),
             radioButtons("scatter_line_group", "Show regression lines by:", choices = group_choices),
             hr(),
             a("Codebook", href = codebook_link, target = "_blank")
           ),
           mainPanel(
             plotOutput("scatter_plot", height = 650)
           )
           )
  )
)

# SERVER
server <- function(input, output) {
  
  # overall title
  output$hist_title <- renderText({
    clean_q_label(attr(data_main[[input$hist_question]], "label"))
  })
  # overall plot
  output$hist_plot <- renderPlot({
    question_var <- input$hist_question
    df <- data.frame(resp = as.character(as_factor(cleaned[[question_var]]))) %>%
      filter(!is.na(resp)) %>% count(resp, name = "n", .drop = FALSE)
    plot_data <- add_plot_value(df, input$hist_mode)
    df <- plot_data$df
    ylab <- plot_data$ylab
    resp_labels <- get_response_labels(question_var)
    df$resp <- factor(df$resp, levels = resp_labels$levels)
    
    ggplot(df, aes(x = resp, y = value)) + geom_col() + coord_flip() +
      scale_x_discrete(labels = resp_labels$labels) +
      labs(x = NULL, y = ylab, caption = paste0("N = ", sum(df$n))) +
      theme_minimal(base_size = 12)
  }, res = 100)
  # overall means table
  output$hist_mean_table <- renderTable({
    cleaned %>%
      mutate(response = as.numeric(.data[[input$hist_question]])) %>%
      filter(!is.na(response)) %>%
      summarise_response() %>%
      mutate(group = "Overall", .before = 1)
  })
  
  
  # sbc title
  output$sbc_title <- renderText({
    clean_q_label(attr(data_main[[input$sbc_question]], "label"))
  })
  # sbc plot
  output$sbc_plot <- renderPlot({
    question_var <- input$sbc_question
    group_var <- input$sbc_group
    df <- data.frame(
      resp = as.character(as_factor(cleaned[[question_var]])),
      group = apply_order(cleaned[[group_var]], group_var)
    ) %>%
      filter(!is.na(resp), !is.na(group)) %>%
      count(resp, group, name = "n", .drop = FALSE)
    
    plot_data <- add_plot_value(df, input$sbc_mode)
    df <- plot_data$df
    ylab <- plot_data$ylab
    
    resp_labels <- get_response_labels(question_var)
    df$resp <- factor(df$resp, levels = resp_labels$levels)
    
    ggplot(df, aes(x = resp, y = value, fill = group)) + 
      geom_col(position = "stack") + coord_flip() +
      scale_x_discrete(labels = resp_labels$labels) +
      scale_fill_brewer(palette = "BuPu", na.translate = FALSE) +
      labs(x = NULL, y = ylab, fill = get_group_label(group_var),
           caption = paste0("N = ", sum(df$n))) +
      theme_minimal(base_size = 12)
  }, res = 100)
  # sbc means table
  output$sbc_mean_table <- renderTable({
    question_var <- input$sbc_question
    group_var <- input$sbc_group
    d <- cleaned %>%
      mutate(
        response = as.numeric(.data[[question_var]]),
        group = apply_order(.data[[group_var]], group_var)
      ) %>%
      filter(!is.na(response), !is.na(group))
    # split into groups
    group_stats <- d %>%
      group_by(group) %>%
      summarise_response()
    #overall
    overall <- d %>%
      summarise_response() %>%
      mutate(group = "Overall", .before = 1)
    bind_rows(group_stats, overall)
  })
  
  # scatter plot
  output$scatter_plot <- renderPlot({
    xq <- input$scatter_question_x
    yq <- input$scatter_question_y
    group_var <- input$scatter_line_group
    d <- cleaned %>%
      mutate(
        x_value = as.numeric(.data[[xq]]),
        y_value = as.numeric(.data[[yq]]),
        line_group = apply_order(.data[[group_var]], group_var)
      ) %>%
      filter(!is.na(x_value), !is.na(y_value), !is.na(line_group))
    x_vl <- attr(data_main[[xq]], "labels")
    y_vl <- attr(data_main[[yq]], "labels")
    x_map <- setNames(paste0(x_vl, " = ", names(x_vl)), as.character(x_vl))
    y_map <- setNames(paste0(y_vl, " = ", names(y_vl)), as.character(y_vl))
    
    ggplot(d, aes(x = x_value, y = y_value, color = line_group)) +
      geom_jitter(width = 0.15, height = 0.15, alpha = 0.25) +
      geom_smooth(method = "lm", se = FALSE) +
      scale_color_brewer(palette = "BuPu", na.translate = FALSE) +
      scale_x_continuous(
        breaks = as.numeric(x_vl),
        labels = function(x) unname(x_map[as.character(x)])
      ) +
      scale_y_continuous(
        breaks = as.numeric(y_vl),
        labels = function(y) unname(y_map[as.character(y)])
      ) +
      labs(x = xq, y = yq, color = get_group_label(group_var),
           caption = paste0("N = ", nrow(d))) +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  }, res = 100)
}

shinyApp(ui, server)


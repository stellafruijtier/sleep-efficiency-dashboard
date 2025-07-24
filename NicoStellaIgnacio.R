# --- Load Libraries ---
library(shiny)
library(plotly)
library(dplyr)
library(lubridate)
library(DT)
library(readr)
library(tidyr)
library(shinyWidgets)
library(bslib)

# --- Load and Prepare Data ---
sleep_data <- read_csv("~/Desktop/Sleep_Efficiency.csv") %>%
  drop_na() %>%
  mutate(
    Gender = as.factor(Gender),
    Bedtime = parse_date_time(Bedtime, orders = c("ymd HMS", "HMS")),
    `Wakeup time` = parse_date_time(`Wakeup time`, orders = c("ymd HMS", "HMS")),
    `Smoking status` = as.factor(`Smoking status`)
  )

# --- Precompute min/max/levels for UI sliders ---
age_min <- floor(min(sleep_data$Age, na.rm = TRUE))
age_max <- ceiling(max(sleep_data$Age, na.rm = TRUE))
alcohol_min <- floor(min(sleep_data$`Alcohol consumption`, na.rm = TRUE))
alcohol_max <- ceiling(max(sleep_data$`Alcohol consumption`, na.rm = TRUE))
caffeine_min <- floor(min(sleep_data$`Caffeine consumption`, na.rm = TRUE))
caffeine_max <- ceiling(max(sleep_data$`Caffeine consumption`, na.rm = TRUE))
exercise_min <- floor(min(sleep_data$`Exercise frequency`, na.rm = TRUE))
exercise_max <- ceiling(max(sleep_data$`Exercise frequency`, na.rm = TRUE))
gender_choices <- c("All", levels(sleep_data$Gender))
smoking_choices <- c("All", levels(sleep_data$`Smoking status`))

# --- Enhanced Theme Colors ---
primary_color <- "#2C3E50"    # Dark blue
accent_color  <- "#27AE60"   # Green
accent_color2 <- "#3498DB"   # Light blue
accent_color3 <- "#F39C12"   # Orange
bg_color      <- "#F5F7FA"   # Light gray background
card_bg_color <- "#FFFFFF"    # White card background
border_color  <- "#E0E6ED"   # Light border



# --- Define UI ---
ui <- fluidPage(
  theme = bs_theme(
    bg = bg_color, 
    fg = primary_color, 
    primary = accent_color,
    base_font = font_google("Montserrat"),
    heading_font = font_google("Montserrat"),
    font_scale = 0.95
  ),
  
  tags$head(
    tags$style(HTML("
      * { font-family: 'Montserrat', sans-serif !important; }
      body { color: #333; line-height: 1.6; }
      .container-fluid { padding: 20px 30px; }
      .app-header {
        background: linear-gradient(135deg, #2C3E50 0%, #4CA1AF 100%);
        color: white;
        padding: 15px 25px;
        border-radius: 12px;
        margin-bottom: 20px;
        box-shadow: 0 4px 15px rgba(0, 0, 0, 0.1);
      }
      .app-title { font-weight: 600; letter-spacing: 0.5px; margin-left: 10px; }
      .plot-container {
        border: none;
        padding: 30px;
        margin-bottom: 25px;
        border-radius: 12px;
        background-color: transparent;
        box-shadow: none;
        transition: all 0.3s ease;
        border-top: 4px solid #27AE60;
        height: 450px;
      }
      .alcohol-plot { height: 500px; }
      .alcohol-boxplot { height: 550px; }
      .plot-container:hover {
        box-shadow: 0px 10px 25px rgba(0, 0, 0, 0.12);
        transform: translateY(-3px);
      }
      .plot-primary { border-top-color: #2C3E50; }
      .plot-accent { border-top-color: #27AE60; }
      .plot-accent2 { border-top-color: #3498DB; }
      .plot-accent3 { border-top-color: #F39C12; }
      .nav-tabs { border-bottom: 2px solid #E0E6ED; margin-bottom: 25px; }
      .nav-tabs .nav-link {
        border: none; color: #7F8C8D; font-weight: 500; padding: 10px 20px; border-radius: 0;
      }
      .nav-tabs .nav-link.active {
        color: #27AE60; border-bottom: 3px solid #27AE60; background-color: transparent;
      }
      .intro-card {
        background-color: white; border-radius: 12px; padding: 20px; margin-bottom: 25px;
        box-shadow: 0px 4px 15px rgba(0, 0, 0, 0.08); border-left: 4px solid #3498DB;
      }
      .big-idea-box {
        background: linear-gradient(135deg, #27AE60 0%, #2ecc71 100%);
        color: white; padding: 15px 20px; border-radius: 12px; margin-bottom: 25px;
        font-size: 16px; font-weight: 500; box-shadow: 0px 4px 15px rgba(39, 174, 96, 0.3);
      }
      .well {
        background-color: white; border: none; border-radius: 12px;
        box-shadow: 0px 4px 15px rgba(0, 0, 0, 0.08); padding: 20px;
      }
      .well h4 {
        color: #2C3E50; border-bottom: 2px solid #E0E6ED; padding-bottom: 10px;
        margin-bottom: 20px; font-weight: 600;
      }
      .irs-bar { background-color: #27AE60; }
      .irs-from, .irs-to, .irs-single { background-color: #27AE60; }
      .btn-primary {
        background-color: #27AE60; border-color: #27AE60; border-radius: 6px;
      }
      .btn-primary:hover { background-color: #219251; border-color: #219251; }
      .dataTables_wrapper {
        padding: 15px; background-color: white; border-radius: 12px;
        box-shadow: 0px 4px 15px rgba(0, 0, 0, 0.08);
      }
      .dataTable { border-collapse: collapse !important; }
      .dataTable thead th {
        background-color: #F5F7FA; color: #2C3E50; border-bottom: 2px solid #E0E6ED !important;
        font-weight: 600;
      }
      .dataTable tbody td { border-top: 1px solid #E0E6ED !important; }
      h3 {
        color: #2C3E50; margin-top: 30px; margin-bottom: 20px; font-weight: 600;
        border-left: 4px solid #27AE60; padding-left: 12px;
      }
      .selectize-input { font-family: 'Montserrat', sans-serif !important; }
      input, button, select, optgroup, textarea, label, .selectize-dropdown {
        font-family: 'Montserrat', sans-serif !important;
      }
      .js-plotly-plot .plotly .main-svg text { font-family: 'Montserrat', sans-serif !important; }
      .js-plotly-plot, .plot-container { width: 100%; }
    "))
  ),
  
  # --- App Header ---
  div(class = "app-header",
      div(
        style = "display: flex; align-items: center;",
        icon("bed", class = "fa-2x"),
        span(class = "app-title", "Sleep Efficiency Dashboard for Wellness Coaching")
      )
  ),
  
  div(class = "big-idea-box",
      icon("lightbulb", class = "fa-lg", style = "margin-right:10px;"),
      "Wellness coaches can use data to improve sleep efficiency by helping clients adjust key lifestyle habits."
  ),
  
  div(class = "intro-card",
      HTML(
        "<b>Who is this dashboard for?</b> Wellness coaches, sleep researchers, and anyone interested in healthy sleep.<br>
        <b>Why use it?</b> Discover how age, gender, and lifestyle factors like caffeine, alcohol, exercise, and smoking status impact sleep efficiency.<br>
        <b>How to use:</b> Filter the data, explore the plots, and use the insights to guide personalized sleep coaching."
      )
  ),
  
  tabsetPanel(
    type = "tabs",
    
    # --- Overview Tab ---
    tabPanel("Overview",
             sidebarLayout(
               sidebarPanel(
                 h4("Demographic Filters"),
                 pickerInput("gender", "Select Gender:", choices = gender_choices, selected = "All",
                             options = list(style = "btn-primary")),
                 pickerInput("smoke", "Smoking Status:", choices = smoking_choices, selected = "All",
                             options = list(style = "btn-primary")),
                 sliderInput("age", "Age Range:", min = age_min, max = age_max, value = c(age_min, age_max)),
                 sliderInput("alcohol", "Alcohol Consumption (units/day):", min = alcohol_min, max = alcohol_max, value = c(alcohol_min, alcohol_max)),
                 sliderInput("caffeine", "Caffeine (mg/day):", min = caffeine_min, max = caffeine_max, value = c(caffeine_min, caffeine_max)),
                 sliderInput("exercise", "Exercise Frequency (per week):", min = exercise_min, max = exercise_max, value = c(exercise_min, exercise_max))
               ),
               
               mainPanel(
                 h3("Sleep & Lifestyle Factors"),
                 fluidRow(
                   column(6, div(class = "plot-container plot-primary", plotlyOutput("ageDistPlot"))),
                   column(6, div(class = "plot-container plot-accent", plotlyOutput("alcoholDistPlot")))
                 ),
                 fluidRow(
                   column(6, div(class = "plot-container plot-accent3", plotlyOutput("smokingStatusPlot"))),
                   column(6, div(class = "plot-container plot-accent2", plotlyOutput("caffeineBoxPlot")))
                 ),
                 fluidRow(
                   column(6, div(class = "plot-container plot-accent2", plotlyOutput("exerciseBoxPlot"))),
                   column(6, div(class = "plot-container plot-accent", plotlyOutput("bedtimeDistPlot")))
                 ),
                 hr(),
                 h3("Sleep Data Table"),
                 DTOutput("sleepTable")
               )
             )
    ),
    
    # --- Alcohol & Sleep Analysis Tab ---
    tabPanel("Alcohol & Sleep Analysis",
             sidebarLayout(
               sidebarPanel(
                 h4("Filter Options"),
                 pickerInput("gender2", "Select Gender:", choices = gender_choices, selected = "All",
                             options = list(style = "btn-primary")),
                 pickerInput("smoke2", "Smoking Status:", choices = smoking_choices, selected = "All",
                             options = list(style = "btn-primary")),
                 sliderInput("alcohol", "Alcohol Consumption (units/day):", min = alcohol_min, max = alcohol_max, value = c(alcohol_min, alcohol_max)),
                 sliderInput("age2", "Age Range:", min = age_min, max = age_max, value = c(age_min, age_max))
               ),
               mainPanel(
                 div(class = "plot-container plot-primary alcohol-boxplot", plotlyOutput("alcoholBoxplot")),
                 div(class = "plot-container plot-accent alcohol-plot", plotlyOutput("groupedBarPlot")),
                 div(class = "plot-container plot-accent3 alcohol-plot", plotlyOutput("alcoholSleepLinePlot"))
               )
             )
    ),
    
    tabPanel("Sleep Analysis + Awakenings",
             sidebarLayout(
               sidebarPanel(
                 h4("Filter Options"),
                 sliderInput("age", "Age Range:", 
                             min = 1, max = 100, value = c(1, 100)),
                 sliderInput("awakenings", "Number of Awakenings:", 
                             min = 0, max = 10, value = c(0, 10)),
                 sliderInput("sleep_efficiency", "Sleep Efficiency Range:", 
                             min = 0.0, max = 1.0, value = c(0.0, 1.0), step = 0.01),
                 pickerInput("smoking", "Smoking Status:", 
                             choices = c("Yes", "No"), selected = c("Yes", "No"),
                             options = list(style = "btn-primary", `actions-box` = TRUE)),
                 downloadButton("downloadData", "Download Filtered Data", 
                                class = "btn-primary")
               ),
               mainPanel(
                 div(class = "plot-container plot-primary",
                     plotlyOutput("scatterplot", height = "350px")),
                 div(class = "plot-container plot-accent",
                     plotOutput("boxplot", height = "350px"))
               )
             )
    ),
    
    
    # --- Exercise & Sleep Analysis Tab ---
    tabPanel("Exercise & Sleep Analysis",
             sidebarLayout(
               sidebarPanel(
                 h4("Filter Options"),
                 pickerInput("gender", "Select Gender:", choices = gender_choices, selected = "All",
                             options = list(style = "btn-primary")),
                 pickerInput("smoke", "Smoking Status:", choices = smoking_choices, selected = "All",
                             options = list(style = "btn-primary")),
                 sliderInput("age", "Age Range:", min = age_min, max = age_max, value = c(age_min, age_max)),
                 sliderInput("exercise", "Exercise Frequency (per week):", 
                             min = exercise_min, max = exercise_max, value = c(exercise_min, exercise_max))
               ),
               mainPanel(
                 div(class = "plot-container plot-primary exercise-boxplot", 
                     plotlyOutput("exerciseBoxplot")),
                 div(class = "plot-container plot-accent exercise-plot", 
                     plotlyOutput("exerciseDurationPlot")),
                 div(class = "plot-container plot-accent2 exercise-plot", 
                     plotlyOutput("exerciseEfficiencyPlot"))
               )
             )
    )
  )
)





# --- Define Server ---
server <- function(input, output) {
  
  # Helper function to wrap long titles into two lines if needed
  wrap_title <- function(title, max_length = 30) {
    if (nchar(title) > max_length) {
      mid_point <- floor(nchar(title) / 2)
      for (i in 1:10) {
        if (mid_point + i <= nchar(title) && substr(title, mid_point + i, mid_point + i) == " ") {
          return(paste0(substr(title, 1, mid_point + i - 1), "<br>", substr(title, mid_point + i + 1, nchar(title))))
        }
        if (mid_point - i > 0 && substr(title, mid_point - i, mid_point - i) == " ") {
          return(paste0(substr(title, 1, mid_point - i - 1), "<br>", substr(title, mid_point - i + 1, nchar(title))))
        }
      }
    }
    return(title)
  }
  
  filtered_data <- reactive({
    sleep_data %>%
      filter(
        (input$gender == "All" | Gender == input$gender),
        (input$smoke == "All" | `Smoking status` == input$smoke),
        between(Age, input$age[1], input$age[2]),
        between(`Alcohol consumption`, input$alcohol[1], input$alcohol[2]),
        between(`Caffeine consumption`, input$caffeine[1], input$caffeine[2]),
        between(`Exercise frequency`, input$exercise[1], input$exercise[2])
      )
  })
  
  output$ageDistPlot <- renderPlotly({
    plot_ly(filtered_data(), x = ~Age, type = "histogram", nbinsx = 20,
            marker = list(color = primary_color, line = list(color = "white", width = 0.5))) %>%
      layout(
        title = list(text = wrap_title("Age Distribution"), font = list(size = 16, color = "#2C3E50", family = "Montserrat")),
        xaxis = list(title = "Age", showgrid = TRUE, gridcolor = "#E0E6ED"),
        yaxis = list(title = "Count", showgrid = TRUE, gridcolor = "#E0E6ED"),
        plot_bgcolor = "#FFFFFF",
        paper_bgcolor = "#FFFFFF",
        margin = list(l = 60, r = 40, b = 80, t = 100, pad = 10),
        autosize = TRUE
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  output$alcoholDistPlot <- renderPlotly({
    plot_ly(filtered_data(), x = ~`Alcohol consumption`, type = "histogram", nbinsx = 20,
            marker = list(color = accent_color, line = list(color = "white", width = 0.5))) %>%
      layout(
        title = list(text = wrap_title("Alcohol Consumption Distribution"), font = list(size = 16, color = "#2C3E50", family = "Montserrat")),
        xaxis = list(title = "Alcohol (units/day)", showgrid = TRUE, gridcolor = "#E0E6ED"),
        yaxis = list(title = "Count", showgrid = TRUE, gridcolor = "#E0E6ED"),
        plot_bgcolor = "#FFFFFF",
        paper_bgcolor = "#FFFFFF",
        margin = list(l = 60, r = 40, b = 80, t = 100, pad = 10),
        autosize = TRUE
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  output$smokingStatusPlot <- renderPlotly({
    df_plot <- filtered_data() %>% count(`Smoking status`)
    plot_ly(df_plot, x = ~`Smoking status`, y = ~n, type = 'bar',
            marker = list(color = accent_color3, line = list(color = "white", width = 0.5))) %>%
      layout(
        title = list(text = wrap_title("Smoking Status Distribution"), font = list(size = 16, color = "#2C3E50", family = "Montserrat")),
        xaxis = list(title = "Smoking Status", showgrid = FALSE),
        yaxis = list(title = "Count", showgrid = TRUE, gridcolor = "#E0E6ED"),
        plot_bgcolor = "#FFFFFF",
        paper_bgcolor = "#FFFFFF",
        margin = list(l = 60, r = 40, b = 80, t = 100, pad = 10),
        autosize = TRUE
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  output$caffeineBoxPlot <- renderPlotly({
    plot_ly(filtered_data(), x = ~`Caffeine consumption`, type = "histogram", nbinsx = 20,
            marker = list(color = accent_color2, line = list(color = "white", width = 0.5))) %>%
      layout(
        title = list(text = wrap_title("Caffeine Consumption Distribution"), font = list(size = 16, color = "#2C3E50", family = "Montserrat")),
        xaxis = list(title = "Caffeine (mg/day)", showgrid = TRUE, gridcolor = "#E0E6ED"),
        yaxis = list(title = "Number of People", showgrid = TRUE, gridcolor = "#E0E6ED"),
        plot_bgcolor = "#FFFFFF",
        paper_bgcolor = "#FFFFFF",
        margin = list(l = 60, r = 40, b = 80, t = 100, pad = 10),
        autosize = TRUE
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  output$exerciseBoxPlot <- renderPlotly({
    exercise_grouped <- filtered_data() %>%
      mutate(Exercise_Group = case_when(
        `Exercise frequency` == 0 ~ "None",
        `Exercise frequency` <= 2 ~ "1-2 times/week",
        `Exercise frequency` <= 4 ~ "3-4 times/week",
        TRUE ~ "5+ times/week"
      )) %>%
      count(Exercise_Group) %>%
      mutate(Exercise_Group = factor(Exercise_Group, levels = c("None", "1-2 times/week", "3-4 times/week", "5+ times/week")))
    
    plot_ly(exercise_grouped, x = ~Exercise_Group, y = ~n, type = 'bar',
            marker = list(color = accent_color2, line = list(color = "white", width = 0.5))) %>%
      layout(
        title = list(text = wrap_title("Exercise Frequency Groups"), font = list(size = 16, color = "#2C3E50", family = "Montserrat")),
        xaxis = list(title = "Exercise Group", showgrid = FALSE),
        yaxis = list(title = "Count", showgrid = TRUE, gridcolor = "#E0E6ED"),
        plot_bgcolor = "#FFFFFF",
        paper_bgcolor = "#FFFFFF",
        margin = list(l = 60, r = 40, b = 80, t = 100, pad = 10),
        autosize = TRUE
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  output$bedtimeDistPlot <- renderPlotly({
    plot_ly(filtered_data(), x = ~hour(Bedtime), type = "histogram", nbinsx = 24,
            marker = list(color = accent_color, line = list(color = "white", width = 0.5))) %>%
      layout(
        title = list(text = wrap_title("Bedtime Hour Distribution"), font = list(size = 16, color = "#2C3E50", family = "Montserrat")),
        xaxis = list(title = "Hour of Bedtime", showgrid = TRUE, gridcolor = "#E0E6ED"),
        yaxis = list(title = "Count", showgrid = TRUE, gridcolor = "#E0E6ED"),
        plot_bgcolor = "#FFFFFF",
        paper_bgcolor = "#FFFFFF",
        margin = list(l = 60, r = 40, b = 80, t = 100, pad = 10),
        autosize = TRUE
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  output$sleepTable <- renderDT({
    datatable(
      filtered_data(), 
      options = list(
        pageLength = 10,
        autoWidth = TRUE,
        scrollX = TRUE,
        columnDefs = list(list(className = 'dt-center', targets = '_all')),
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf')
      ),
      rownames = FALSE,
      class = 'cell-border stripe hover'
    )
  })
  
  

  
  
  
  
# NICO WHOLE TAB
  
  filtered_data2 <- reactive({
    sleep_data %>%
      filter(
        (input$gender2 == "All" | Gender == input$gender2),
        (input$smoke2 == "All" | `Smoking status` == input$smoke2),
        between(Age, input$age2[1], input$age2[2]),
        between(`Alcohol consumption`, input$alcohol[1], input$alcohol[2])
      )
  })
  
  output$alcoholBoxplot <- renderPlotly({
    plot_data <- filtered_data2() %>%
      mutate(Alcohol_Level = case_when(
        `Alcohol consumption` <= 1 ~ "Low",
        `Alcohol consumption` <= 3 ~ "Moderate",
        TRUE ~ "High"
      )) %>%
      mutate(Alcohol_Level = factor(Alcohol_Level, levels = c("Low", "Moderate", "High")))
    
    if (max(plot_data$`Sleep efficiency`, na.rm = TRUE) <= 1.1) {
      plot_data$`Sleep efficiency` <- plot_data$`Sleep efficiency` * 100
    }
    
    plot_ly(plot_data, x = ~Alcohol_Level, y = ~`Sleep efficiency`, type = "box", 
            color = ~Alcohol_Level,
            colors = c("#27AE60", "#F39C12", "#E74C3C"),
            boxpoints = "all", jitter = 0, pointpos = 0,
            line = list(width = 3),
            marker = list(size = 2, opacity = 1)) %>%
      layout(
        title = list(text = wrap_title("Sleep Efficiency vs Alcohol Consumption Level"), 
                     font = list(size = 18, color = "#2C3E50", family = "Montserrat")),
        xaxis = list(
          title = list(text = "Alcohol Consumption Level", font = list(size = 14)),
          type = "category",
          categoryorder = "array", 
          categoryarray = c("Low", "Moderate", "High"),
          showgrid = FALSE,
          tickfont = list(size = 12)
        ),
        yaxis = list(
          title = list(text = "Sleep Efficiency (%)", font = list(size = 14)), 
          showgrid = TRUE, 
          gridcolor = "#E0E6ED",
          tickfont = list(size = 12)
        ),
        plot_bgcolor = "#FFFFFF",
        paper_bgcolor = "#FFFFFF",
        boxmode = "group",
        boxgap = 0,
        showlegend = FALSE,
        margin = list(l = 80, r = 40, b = 80, t = 100, pad = 10),
        autosize = TRUE
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  output$groupedBarPlot <- renderPlotly({
    plot_data <- filtered_data2() %>%
      mutate(Alcohol_Level = case_when(
        `Alcohol consumption` <= 1 ~ "Low",
        `Alcohol consumption` <= 3 ~ "Moderate",
        TRUE ~ "High"
      )) %>%
      mutate(Alcohol_Level = factor(Alcohol_Level, levels = c("Low", "Moderate", "High"))) %>%
      group_by(Alcohol_Level, Gender) %>%
      summarise(Avg_Sleep_Efficiency = mean(`Sleep efficiency`, na.rm = TRUE), .groups = 'drop')
    
    if (max(plot_data$Avg_Sleep_Efficiency, na.rm = TRUE) <= 1.1) {
      plot_data$Avg_Sleep_Efficiency <- plot_data$Avg_Sleep_Efficiency * 100
    }
    
    plot_ly(plot_data, x = ~Alcohol_Level, y = ~Avg_Sleep_Efficiency, color = ~Gender, type = 'bar', 
            barmode = 'group',
            colors = c("Male" = "#97d5c0", "Female" = "#b0bcdb")) %>%
      layout(
        title = list(text = wrap_title("Sleep Efficiency vs Alcohol Consumption by Gender"), 
                     font = list(size = 18, color = "#2C3E50", family = "Montserrat")),
        xaxis = list(
          title = list(text = "Alcohol Consumption Level", font = list(size = 14)),
          showgrid = FALSE,
          tickfont = list(size = 12)
        ),
        yaxis = list(
          title = list(text = "Average Sleep Efficiency (%)", font = list(size = 14)), 
          showgrid = TRUE, 
          gridcolor = "#E0E6ED",
          tickfont = list(size = 12)
        ),
        plot_bgcolor = "#FFFFFF",
        paper_bgcolor = "#FFFFFF",
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = -0.3,
          font = list(size = 12, family = "Montserrat")
        ),
        margin = list(l = 80, r = 40, b = 80, t = 100, pad = 10),
        autosize = TRUE
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  output$alcoholSleepLinePlot <- renderPlotly({
    plot_data <- filtered_data2() %>%
      group_by(`Alcohol consumption`, Gender) %>%
      summarise(Avg_Sleep_Duration = mean(`Sleep duration`, na.rm = TRUE), .groups = 'drop')
    
    plot_ly(plot_data, 
            x = ~`Alcohol consumption`, 
            y = ~Avg_Sleep_Duration,
            color = ~Gender,
            colors = c("Male" = "#97d5c0", "Female" = "#b0bcdb"),
            type = "scatter",
            mode = "lines+markers",
            marker = list(size = 8),
            line = list(width = 3)) %>%
      layout(
        title = list(text = "Sleep Duration vs Alcohol Consumption", 
                     font = list(size = 18, color = primary_color)),
        xaxis = list(title = "Alcohol Consumption (units/day)", 
                     gridcolor = "#E0E6ED",
                     showgrid = TRUE),
        yaxis = list(title = "Average Sleep Duration (hours)", 
                     gridcolor = "#E0E6ED"),
        plot_bgcolor = card_bg_color,
        paper_bgcolor = card_bg_color,
        legend = list(orientation = "h", 
                      x = 0.5, 
                      y = -0.3,
                      xanchor = "center"),
        margin = list(l= 80, r = 40, t = 100, b = 80, pad = 10)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  
  
  
  # STELLA WHOLE TAB
  filtered_data3 <- reactive({
    sleep_data %>%
      filter(
        (input$gender == "All" | Gender == input$gender),
        (input$smoke == "All" | `Smoking status` == input$smoke),
        between(Age, input$age[1], input$age[2]),
        between(`Exercise frequency`, input$exercise[1], input$exercise[2])
      )
  })
  
  
  # Plot 1 : Sleep duration vs Exercise Frequency boxplot
  
  output$exerciseBoxplot <- renderPlotly({
    plot_data <- filtered_data3() %>%
      mutate(Exercise_Group = case_when(
        `Exercise frequency` == 0 ~ "None",
        `Exercise frequency` <= 2 ~ "1-2 times/week",
        `Exercise frequency` <= 4 ~ "3-4 times/week",
        TRUE ~ "5+ times/week"
      )) %>%
      mutate(Exercise_Group = factor(Exercise_Group, 
                                     levels = c("None", "1-2 times/week", "3-4 times/week", "5+ times/week")))
    
    if (max(plot_data$`Sleep duration`, na.rm = TRUE) <= 1.1) {
      plot_data$`Sleep duration` <- plot_data$`Sleep duration` * 100
    }
    
    
    plot_ly(plot_data, x = ~Exercise_Group, y = ~`Sleep duration`, type = "box", 
            color = ~Exercise_Group,
            colors = c("#3498DB", "#27AE60", "#E74C3C", "#F39C12"),
            boxpoints = "all", jitter = 0, pointpos = 0,
            line = list(width = 3),
            marker = list(size = 2, opacity = 1)) %>%
      layout(
        title = list(text = "Sleep Duration vs Exercise Frequency", 
                     font = list(size = 18, color = "#2C3E50", family = "Montserrat")),
        xaxis = list(
          title = list(text = "Exercise Frequency", font = list(size = 14)),
          type = "category",
          categoryorder = "array",
          categoryarray = c("Low", "Moderate", "High"),
          showgrid = FALSE,
          tickfont = list(size = 12)
        ),
        yaxis = list(
          title = list(text = "Sleep Duration (hours)", font = list(size = 14)), 
          showgrid = TRUE, 
          gridcolor = "#E0E6ED",
          tickfont = list(size = 12)
        ),
        plot_bgcolor = "#FFFFFF",
        paper_bgcolor = "#FFFFFF",
        boxmode = "group",
        boxgap = 0,
        showlegend = FALSE,
        margin = list(l = 80, r = 40, b = 80, t = 100, pad = 10),
        autosize = TRUE
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  
  
  
  # Plot 2: Sleep duration vs Exercise Frequency scatter + line
  
  output$exerciseDurationPlot <- renderPlotly({
    # First, check column names in the actual data
    print(colnames(filtered_data3()))
    
    # Use more robust column name references
    plot_data <- filtered_data3() %>%
      rename(
        exercise_freq = `Exercise frequency`,
        sleep_duration = `Sleep duration`
      ) %>%
      group_by(exercise_freq, Gender) %>%
      summarise(Avg_Sleep_Duration = mean(sleep_duration, na.rm = TRUE), 
                .groups = 'drop') %>%
      filter(!is.na(exercise_freq), !is.na(Avg_Sleep_Duration))
    
    # Check if we have data
    if (nrow(plot_data) == 0) {
      return(plotly_empty(type = "scatter") %>%
               layout(title = "No data available for selected filters"))
    }
    
    # Create plot
    fig <- plot_ly(plot_data, 
                   x = ~exercise_freq, 
                   y = ~Avg_Sleep_Duration,
                   color = ~Gender,
                   colors = c("#3498DB", "#E74C3C"),
                   type = 'scatter',
                   mode = 'lines+markers',
                   marker = list(size = 10, opacity = 0.7))
    
    # Add trend lines if enough points
    genders <- unique(plot_data$Gender)
    for (g in genders) {
      gender_data <- plot_data %>% filter(Gender == g)
      if(nrow(gender_data) >= 3) {
        fig <- fig %>%
          add_trace(data = gender_data,
                    x = ~exercise_freq,
                    y = ~fitted(loess(Avg_Sleep_Duration ~ exercise_freq)),
                    type = 'scatter',
                    mode = 'line+markers',
                    marker = list(size = 10, opacity = 0.7),
                    line = list(width = 3),
                    showlegend = FALSE)
      }
    }
    
    fig %>% layout(
      title = "Sleep Duration vs Exercise Frequency",
      xaxis = list(title = "Exercise Frequency (times/week)"),
      yaxis = list(title = "Average Sleep Duration (hours)"),
      legend = list(orientation = 'h', y = -0.3,
                    x = 0.5,
                    xanchor = "center",
                    y = -2,
                    font = list(size = 12, family = "Montserrat")),
      margin = list(l = 80, r = 40, b = 40, t = 100, pad = 10))
  })
  
  
  # Plot 3: Sleep efficiency vs Exercise Frequency scatter +line
  
  output$exerciseEfficiencyPlot <- renderPlotly({
    plot_data <- filtered_data3() %>%
      group_by(`Exercise frequency`, Gender) %>%
      summarise(Avg_Sleep_Efficiency = mean(`Sleep efficiency`, na.rm = TRUE), .groups = 'drop')
    
    if (max(plot_data$Avg_Sleep_Efficiency, na.rm = TRUE) <= 1.1) {
      plot_data$Avg_Sleep_Efficiency <- plot_data$Avg_Sleep_Efficiency * 100
    }
    
    plot_ly(plot_data, x = ~`Exercise frequency`, y = ~Avg_Sleep_Efficiency, color = ~Gender, type = 'scatter',
            mode = 'line+markers',
            colors = c("#3498DB", "#E74C3C"),
            marker = list(size = 10, opacity = 0.7),
            line = list(width = 3)) %>%
      layout(
        title = list(text = "Sleep Efficiency vs Exercise Frequency", 
                     font = list(size = 18, color = "#2C3E50", family = "Montserrat")),
        xaxis = list(
          title = list(text = "Exercise Frequency (times/week)", font = list(size = 14)),
          showgrid = TRUE,
          gridcolor = "#E0E6ED",
          tickfont = list(size = 12)
        ),
        yaxis = list(
          title = list(text = "Average Sleep Efficiency (%)", font = list(size = 14)), 
          showgrid = TRUE, 
          gridcolor = "#E0E6ED",
          tickfont = list(size = 12)
        ),
        plot_bgcolor = "#FFFFFF",
        paper_bgcolor = "#FFFFFF",
        legend = list(orientation = "h", 
                      x = 0.5, 
                      xanchor = "center", 
                      y = -0.35, 
                      font = list(size = 12, family = "Montserrat")),
        margin = list(l = 80, r = 40, b = 80, t = 100, pad = 10),
        autosize = TRUE
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  
  
  # ignacio
  # Generate and filter data based on inputs
  filtered_data4 <- reactive({
    # Creating a sample dataset for demonstration
    # Replace this with your actual data loading
    set.seed(123)
    data <- data.frame(
      Subject_ID = 1:200,
      Age = sample(1:100, 200, replace = TRUE),
      Awakenings = sample(0:10, 200, replace = TRUE),
      Smoking_status = sample(c("Yes", "No"), 200, replace = TRUE),
      Sleep_efficiency = runif(200, 0, 1),
      Sleep_duration = rnorm(200, 7, 1.5),
      Deep_sleep_percentage = runif(200, 0.1, 0.4)
    )
    
    # Apply filters based on slider inputs
    data %>%
      filter(
        Age >= input$age[1] & Age <= input$age[2],
        Awakenings >= input$awakenings[1] & Awakenings <= input$awakenings[2],
        Sleep_efficiency >= input$sleep_efficiency[1] & Sleep_efficiency <= input$sleep_efficiency[2],
        Smoking_status %in% input$smoking
      )
  })
  
  # Create interactive scatter plot with plotly
  output$scatterplot <- renderPlotly({
    data <- filtered_data4()
    
    p <- ggplot(data, aes(x = Age, y = Sleep_efficiency, 
                          size = Awakenings, color = Smoking_status,
                          text = paste("ID:", Subject_ID, 
                                       "<br>Age:", Age,
                                       "<br>Awakenings:", Awakenings,
                                       "<br>Sleep Efficiency:", round(Sleep_efficiency, 2)))) +
      geom_point(alpha = 0.7) +
      scale_color_manual(values = c("Yes" = "#FF5733", "No" = "#33A1FF")) +
      scale_size_continuous(range = c(2, 8)) +
      labs(
        x = "Age",
        y = "Sleep Efficiency",
        color = "Smoking Status",
        size = "Awakenings"
      ) +
      theme_minimal() +
      theme(
        axis.title = element_text(size = 12),
        legend.position = "bottom"
      )
    
    # Convert to plotly for interactivity
    ggplotly(p, tooltip = "text")
  })
  
  # Create box plot
  output$boxplot <- renderPlot({
    data <- filtered_data4()
    
    # Create age groups for better visualization
    data$Age_Group <- cut(data$Age, 
                          breaks = c(0, 18, 35, 50, 65, 100),
                          labels = c("0-18", "19-35", "36-50", "51-65", "65+"),
                          include.lowest = TRUE)
    
    ggplot(data, aes(x = Age_Group, y = Awakenings, fill = Smoking_status)) +
      geom_boxplot(alpha = 0.7, outlier.shape = 21) +
      scale_fill_manual(values = c("Yes" = "#FF5733", "No" = "#33A1FF")) +
      labs(
        x = "Age Group",
        y = "Number of Awakenings",
        fill = "Smoking Status"
      ) +
      theme_minimal() +
      theme(
        axis.title = element_text(size = 12),
        legend.position = "bottom",
        panel.grid.major.x = element_blank()
      )
  })
  
  # Use DT for better data table display
  output$filtered_data4 <- renderDT({
    DT::datatable(filtered_data4(), 
                  options = list(pageLength = 10, 
                                 searching = TRUE,
                                 scrollX = TRUE))
  })
  
  # Output summary statistics
  output$summary <- renderPrint({
    data <- filtered_data4()
    
    # Create a more detailed summary
    cat("Dataset Summary:\n")
    cat("------------------\n")
    cat("Number of observations:", nrow(data), "\n\n")
    
    cat("Age Summary:\n")
    print(summary(data$Age))
    cat("\n")
    
    cat("Awakenings Summary:\n")
    print(summary(data$Awakenings))
    cat("\n")
    
    cat("Sleep Efficiency Summary:\n")
    print(summary(data$Sleep_efficiency))
    cat("\n")
    
    cat("Smoking Status Distribution:\n")
    print(table(data$Smoking_status))
    cat("\n")
    
    # Calculate correlations
    cat("Correlation between Age and Sleep Efficiency:\n")
    print(cor.test(data$Age, data$Sleep_efficiency))
    cat("\n")
    
    cat("Correlation between Awakenings and Sleep Efficiency:\n")
    print(cor.test(data$Awakenings, data$Sleep_efficiency))
  })
  
  # Download handler for the filtered data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("sleep_data_filtered_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data4(), file, row.names = FALSE)
    }
  )
}

# --- Run App ---
shinyApp(ui = ui, server = server)


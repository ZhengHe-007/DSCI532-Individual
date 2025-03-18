options(repos = c(CRAN = "https://cran.r-project.org"))

library(shiny)
library(tidyverse)
library(plotly)

# Load Data
df <- read_csv("../data/raw/HRR-20.csv", col_types = cols(.default = "c"))  

# Remove the second row (it contains unwanted text)
df <- df[-1, ]

# Clean column names: Remove spaces, commas, and special characters
colnames(df) <- colnames(df) %>%
  gsub(",", "", .) %>%    
  gsub(" ", "_", .) %>%   
  gsub("\\*", "", .)      

# Convert numeric columns
numeric_cols <- c("Total_Hospital_Beds", "Total_ICU_Beds", 
                  "Available_Hospital_Beds", "Available_ICU_Beds",
                  "ICU_Beds_Needed_Six_Months", "ICU_Beds_Needed_Twelve_Months",
                  "ICU_Beds_Needed_Eighteen_Months",
                  "Potentially_Available_ICU_Beds")  

df[numeric_cols] <- df[numeric_cols] %>%
  mutate_all(~ as.numeric(gsub(",", "", .)))  

# UI
ui <- fluidPage(
  titlePanel("Hospital Bed Capacity Dashboard - 20% Infection Rate"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("icu_threshold", "Minimum Available ICU Beds:", 
                  choices = c("None", "50", "100", "150", "200"), selected = "None"),
      
      selectInput("hrr", "Select HRR Region:", 
                  choices = unique(df$HRR), selected = unique(df$HRR)[1]),
      
      sliderInput("time_period", "Select Time Period (Months):", 
                  min = 6, max = 18, value = 6, step = 6)
    ),
    
    mainPanel(
      plotOutput("icuStackedPlot"),  # Pie Chart (Availability vs Demand)
      plotlyOutput("icuGrowthPlot")  # ICU Demand Growth Over Time (With Tooltips)
    )
  )
)

# Server
server <- function(input, output, session) {
  
  observe({
    if (input$icu_threshold == "None") {
      valid_hrr <- unique(df$HRR)  
    } else {
      threshold <- as.numeric(input$icu_threshold)  
      valid_hrr <- df %>%
        filter(Available_ICU_Beds >= threshold) %>%
        pull(HRR) %>%
        unique()
    }
    
    updateSelectInput(session, "hrr", choices = valid_hrr, selected = valid_hrr[1])
  })
  
  icu_demand_data <- reactive({
    df %>%
      filter(HRR == input$hrr) %>%
      select(HRR, Available_ICU_Beds, 
             ICU_Beds_Needed_Six_Months, ICU_Beds_Needed_Twelve_Months, ICU_Beds_Needed_Eighteen_Months) %>%
      pivot_longer(cols = starts_with("ICU_Beds_Needed"), names_to = "Time_Period", values_to = "ICU_Needed") %>%
      mutate(Time_Period = recode(Time_Period, 
                                  "ICU_Beds_Needed_Six_Months" = "6 Months", 
                                  "ICU_Beds_Needed_Twelve_Months" = "12 Months", 
                                  "ICU_Beds_Needed_Eighteen_Months" = "18 Months")) %>%
      mutate(Time_Period = factor(Time_Period, levels = c("6 Months", "12 Months", "18 Months")))
  })
  
  icu_availability_data <- reactive({
    df %>%
      filter(HRR == input$hrr) %>%
      select(HRR, Available_ICU_Beds, Potentially_Available_ICU_Beds) %>%
      pivot_longer(cols = c("Available_ICU_Beds", "Potentially_Available_ICU_Beds"), 
                   names_to = "Time_Period", values_to = "ICU_Available") %>%
      mutate(Time_Period = recode(Time_Period, 
                                  "Available_ICU_Beds" = "6 Months", 
                                  "Potentially_Available_ICU_Beds" = "12 Months")) %>%
      bind_rows(tibble(HRR = input$hrr, Time_Period = "18 Months", ICU_Available = df$Available_ICU_Beds[df$HRR == input$hrr])) %>%
      mutate(Time_Period = factor(Time_Period, levels = c("6 Months", "12 Months", "18 Months")))
  })
  
  pie_chart_data <- reactive({
    icu_demand_data() %>%
      filter(Time_Period == paste(input$time_period, "Months"))  
  })
  
  # **ICU Capacity Plot (Pie Chart) - Showing Percentage Labels**
  output$icuStackedPlot <- renderPlot({
    filtered <- pie_chart_data()
    
    available_icu_beds <- df$Available_ICU_Beds[df$HRR == input$hrr]
    icu_needed <- filtered$ICU_Needed[1]
    
    total_beds <- available_icu_beds + icu_needed
    percentages <- round(c(available_icu_beds, icu_needed) / total_beds * 100, 1)
    
    plot_data <- tibble(
      Category = c("Available ICU Beds", "ICU Beds Needed"),
      Value = c(available_icu_beds, icu_needed),
      Label = paste0(percentages, "%")
    )
    
    ggplot(plot_data, aes(x = "", y = Value, fill = Category)) +
      geom_bar(stat = "identity", width = 1) +
      geom_text(aes(label = Label), position = position_stack(vjust = 0.5), 
                size = 6, color = "black", fontface = "bold") +  
      theme_minimal() +
      coord_polar(theta = "y") +
      labs(title = paste("ICU Bed Availability vs. Demand (", input$time_period, " Months)", sep=""), 
           x = "", y = "") +
      theme(axis.text.x = element_blank(), axis.ticks = element_blank(),
            plot.title = element_text(face = "bold", size = 18, hjust = 0.5, family = "Arial")) +  
      scale_fill_manual(values = c("Available ICU Beds" = "limegreen", "ICU Beds Needed" = "red"))
  })
  
  # **ICU Demand Growth Over Time (Line Chart with Tooltips)**
  output$icuGrowthPlot <- renderPlotly({
    demand_data <- icu_demand_data() %>%
      mutate(Category = "ICU Beds Needed") %>%
      select(Time_Period, Value = ICU_Needed, Category)
    
    availability_data <- icu_availability_data() %>%
      mutate(Category = "Available ICU Beds") %>%
      select(Time_Period, Value = ICU_Available, Category)
    
    combined_data <- bind_rows(demand_data, availability_data)
    
    p <- ggplot(combined_data, aes(x = Time_Period, y = Value, group = Category, color = Category, text = paste(Category, ":", Value))) +
      geom_line(size = 1.5) +
      geom_point(size = 4) +
      scale_color_manual(values = c("ICU Beds Needed" = "red", "Available ICU Beds" = "green")) +
      theme_minimal() +
      labs(title = "Projected ICU Demand vs Availability Over Time", 
           x = "Time Period", 
           y = "ICU Beds") +
      theme(legend.title = element_blank(), legend.position = "top",
            plot.title = element_text(face = "bold", size = 13, hjust = 0.5, family = "Arial"))  
    
    ggplotly(p, tooltip = "text")
  })
}

# Run the app
shinyApp(ui = ui, server = server)

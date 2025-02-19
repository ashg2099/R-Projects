# Function to check and install packages
install_if_missing <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new_packages)) install.packages(new_packages)
}

# List of required packages
required_packages <- c(
  "shiny", "ggplot2", "dplyr", "plotly", "sf", "readr", 
  "lubridate", "RColorBrewer", "zoo", "htmlwidgets", "leaflet"
)

# Install missing packages
install_if_missing(required_packages)

# Loading the required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(sf)
library(readr)
library(lubridate)
library(RColorBrewer)
library(zoo)
library(htmlwidgets)
library(leaflet)

# Load the data
data <- read_csv("DEP_cleaned1.csv")

# Manually create a mapping of state abbreviations to full state names
state_mapping <- data.frame(
  State_abb = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", 
                "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", 
                "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", 
                "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", 
                "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"),
  State_full = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", 
                 "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", 
                 "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", 
                 "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", 
                 "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", 
                 "New Hampshire", "New Jersey", "New Mexico", "New York", 
                 "North Carolina", "North Dakota", "Ohio", "Oklahoma", 
                 "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", 
                 "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", 
                 "Virginia", "Washington", "West Virginia", "Wisconsin", 
                 "Wyoming")
)

# Merge the complaint data with the state mapping
data <- data %>%
  left_join(state_mapping, by = c("State" = "State_abb"))

# Aggregate the complaint counts by state
complaint_data <- data %>%
  group_by(State_full) %>%
  summarise(complaints = n())

# Load US states shapefile
us_states <- st_read("https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json")

# Convert state names in us_states to title case to match the state_mapping format
us_states$name <- tools::toTitleCase(tolower(us_states$name))

# Merge complaint data with shapefile data
merged_data <- us_states %>%
  left_join(complaint_data, by = c("name" = "State_full"))

# Convert Date received to Date type for cumulative plot
data <- data %>%
  mutate(Date_received = dmy(`Date received`)) %>%
  arrange(Date_received)

# Calculate cumulative complaints over time
cumulative_data <- data %>%
  group_by(Date_received) %>%
  summarise(daily_complaints = n()) %>%
  mutate(cumulative_complaints = cumsum(daily_complaints))

# Calculate monthly complaints for cyclic plot
data <- data %>%
  mutate(Month = month(Date_received, label = TRUE, abbr = TRUE),
         Year = year(Date_received))

monthly_data <- data %>%
  group_by(Product, Year, Month) %>%
  summarise(monthly_complaints = n()) %>%
  ungroup()

# Calculate rolling average complaints over time
rolling_avg_data <- cumulative_data %>%
  mutate(rolling_avg_7 = zoo::rollmean(daily_complaints, 7, fill = NA, align = "right"),
         rolling_avg_30 = zoo::rollmean(daily_complaints, 30, fill = NA, align = "right"))

# Calculate month-over-month change
monthly_change_data <- monthly_data %>%
  group_by(Product, Year) %>%
  mutate(monthly_change = (monthly_complaints - lag(monthly_complaints)) / lag(monthly_complaints) * 100) %>%
  ungroup()

# Ensure the Year and Month columns are created and in correct format
data <- data %>%
  mutate(Date_received = mdy(`Date received`)) %>%
  mutate(Year = year(Date_received), Month = month(Date_received, label = TRUE, abbr = TRUE)) %>%
  filter(!is.na(Year), !is.na(Month))

# Get min and max year values for sliders
min_year <- min(monthly_data$Year, na.rm = TRUE)
max_year <- max(monthly_data$Year, na.rm = TRUE)

# Calculate top issues
top_issues_data <- data %>%
  group_by(Issue) %>%
  summarise(issue_count = n()) %>%
  arrange(desc(issue_count)) %>%
  head(10)

# Define UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Roboto:wght@300;400;700&display=swap');

      body {
        background: url('https://s.yimg.com/uu/api/res/1.2/FaX2bTOrJsPgNZvlACgCXw--~B/aD00MDA7dz02MzU7YXBwaWQ9eXRhY2h5b24-/https://media.zenfs.com/en/zacks.com/32389a4625f6ac2960e954af2d7bcd7a.cf.webp') no-repeat center center fixed;
        background-size: cover;
        font-family: 'Roboto', sans-serif;
        margin: 0;
        padding: 0;
        display: flex;
        flex-direction: column;
        height: 100vh;
        overflow-x: hidden;
        color: #333;
        transition: background-color 0.5s ease;
      }
      .sidebar {
        background: rgba(0, 51, 102, 0.9);
        color: white;
        width: 250px;
        padding: 20px;
        position: fixed;
        top: 80px;
        bottom: 0;
        left: 0;
        z-index: 500;
        display: flex;
        flex-direction: column;
        align-items: flex-start;
        border-top-right-radius: 10px;
        border-bottom-right-radius: 10px;
        box-shadow: 2px 0 10px rgba(0, 0, 0, 0.3);
        font-family: 'Roboto', sans-serif;
      }
      .sidebar a {
        color: #ffffff;
        padding: 14px 20px;
        text-decoration: none;
        display: block;
        width: 100%;
        text-align: left;
        transition: background-color 0.3s ease, color 0.3s ease;
        margin-top: 20px;
        border-radius: 5px;
      }
      .sidebar a:hover {
        background-color: #005f99;
        color: #ffffff;
      }
      .sidebar a i {
        margin-right: 10px;
      }
      .main-content {
        margin-left: 270px;
        padding: 20px;
        width: calc(100% - 270px);
        background: rgba(255, 255, 255, 0.85);
        border-radius: 10px;
        color: #333;
        box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);
        overflow-y: auto; 
        transition: box-shadow 0.3s ease;
        flex: 1;
        font-family: 'Roboto', sans-serif;
      }
      .header {
        background: rgba(0, 41, 84, 0.9);
        color: white;
        padding: 10px;
        text-align: center;
        margin: 0;
        width: 100%;
        height: 80px;
        position: fixed;
        top: 0;
        left: 0;
        z-index: 1000;
        display: flex;
        flex-direction: column;
        justify-content: center;
        align-items: center;
        box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);
        border-bottom-left-radius: 10px;
        border-bottom-right-radius: 10px;
        font-family: 'Roboto', sans-serif;
      }
      .header h1, .header p {
        color: #ffffff; 
        font-weight: 700; 
        font-family: 'Roboto', sans-serif;
      }
      .section {
        padding: 20px;
        margin: 100px 0 20px 0;
        border-radius: 10px;
        background: rgba(255, 255, 255, 0.9);
        box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
        transition: transform 0.3s ease, box-shadow 0.3s ease;
        font-family: 'Roboto', sans-serif;
      }
      .section:hover {
        transform: scale(1.02);
        box-shadow: 0 6px 12px rgba(0, 0, 0, 0.2);
      }
      .card {
        background: rgba(255, 255, 255, 0.95);
        padding: 20px;
        margin: 0;
        border-radius: 10px;
        box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
        transition: transform 0.3s ease, box-shadow 0.3s ease;
        width: 100%;
        overflow-x: auto;
        font-family: 'Roboto', sans-serif;
      }
      .card:hover {
        transform: scale(1.02);
        box-shadow: 0 6px 12px rgba(0, 0, 0, 0.2);
      }
      .footer {
        background: rgba(0, 41, 84, 0.9);
        color: white;
        padding: 10px;
        text-align: center;
        margin: 0;
        width: 100%;
        position: fixed;
        bottom: 0;
        left: 0;
        height: 40px;
        z-index: 1000;
        box-shadow: 0 -4px 8px rgba(0, 0, 0, 0.2);
        border-top-left-radius: 10px;
        border-top-right-radius: 10px;
        font-family: 'Roboto', sans-serif;
      }
      h1, h2, h3, h4, h5 {
        color: #1a73e8; 
        font-weight: 700; 
        
      }
      p {
        font-weight: 300; 
        font-family: 'Roboto', sans-serif;
      }
      insight-text {
        animation: fadeIn 1s ease-in-out;
        display: inline-block;  
        vertical-align: top;    
        width: 48%;             
        margin-left: 2%;        
        font-family: 'Roboto', sans-serif;
      }
      .bar-chart {
        width: 48%;             
        display: inline-block;  
        vertical-align: top;    
      }
      @keyframes fadeIn {
        0% { opacity: 0; }
        100% { opacity: 1; }
      }
      .fade-in {
        animation: fadeIn 1s ease-in;
      }
    ")),
    tags$script(HTML("
      $(document).ready(function(){
        $('a[href^=\"#\"]').on('click', function(event) {
          var target = $(this.getAttribute('href'));
          if( target.length ) {
            event.preventDefault();
            $('html, body').stop().animate({
              scrollTop: target.offset().top - 60
            }, 1000);
          }
        });
      });
    "))
  ),
  
  # Sidebar navigation with links to each section 
  div(class = "sidebar",
      a(href="#about-section", tags$img(src="https://logosarchive.com/wp-content/uploads/2021/08/Bank-of-America-icon.svg", height="10px", style="margin-right: 10px;"), "Learn more about us"),
      a(href="#state-analysis-section", icon("map"), "View state-wise data"),
      a(href="#month-over-month-section", icon("calendar"), "See monthly changes"),
      a(href="#complaints-trend-section", icon("chart-line"), "Check the trends")
  ),
  
  # Main content and each individual section's style and layout
  div(class = "main-content",
      div(class = "header", id="home",
          div(class = "header-content",
              h1("Consumer Complaints Analysis for Bank of America (2017-2023)"),
          )
      ),
      div(class = "section", id="about-section",
          div(class = "card",
              h2("About Bank of America"),
              img(src="https://s3.us-east-1.amazonaws.com/assets-v2.highwoods.com/content-refresh/images/properties/_propertyHero/DJI_0603-HDR-Edit.jpg", height="300px", width="1100px", style="display:block; display: flex; margin-left:auto; margin-right:auto;", class="fade-in"),
              p(" "),
              p("Bank of America Corporation is an American multinational investment bank and financial services company headquartered in Charlotte, North Carolina. Founded in 1904, it has grown significantly over the years to become one of the world's largest financial institutions. The bank serves individual consumers, small and middle-market businesses, and large corporations with a comprehensive range of banking, investing, asset management, and other financial and risk management products and services. As a leader in the financial industry, Bank of America's mission is to help make financial lives better by connecting clients and communities to the resources they need to be successful. The bank operates in all 50 states, the District of Columbia, U.S. Virgin Islands, Puerto Rico, and more than 35 countries."),
              h3("Growth and Achievements"),
              tags$ul(style = "font-family: 'Roboto', sans-serif;",
                      tags$li("Ranked 25th on the Fortune 500 list of largest companies in the United States by total revenue."),
                      tags$li("Recognized for its leadership in environmental, social, and governance (ESG) issues."),
                      tags$li("Invested billions in sustainable finance initiatives to address climate change."),
                      tags$li("Strong commitment to diversity and inclusion, supporting a diverse workforce and supplier base."),
                      tags$li("Extensive philanthropic efforts through the Bank of America Charitable Foundation.")
              ),
              h3("Journey Through Consumer Complaints: A Story of Insights and Trends"),
              p("Join us on a detailed exploration of consumer feedback, where each section of our analysis unravels a part of the broader narrative. This journey begins with a granular, state-by-state examination, transitions through a detailed month-over-month comparison, and culminates in a comprehensive trend analysis that spans years. Through this interactive narrative, we aim to provide a deep understanding of the consumer experience, revealing patterns and trends that can inform strategic decisions and improvements."),
              p("Dive into the state-wise analysis to uncover how consumer complaints vary across different regions. Move on to the month-over-month analysis to see how the volume of complaints changes over time and discover any significant fluctuations. Finally, explore the complaints trend analysis to gain insights into long-term patterns and seasonal trends. Each section offers interactive elements that allow you to engage with the data and draw your own conclusions."),
              p("Ready to explore? Start with the state-wise analysis and navigate through the sections to uncover the full story. ",
                strong("Click on the links in the sidebar to begin the journey.")
              )
          )
      ),
      div(class = "section", id="state-analysis-section",
          div(class = "card",
              h2("State-wise analysis"),
              p("Our story starts at the state level, where we delve into the geographic distribution of complaints. By selecting a state, or comparing multiple states, we gain insights into regional patterns and identify which states stand out in terms of the number of complaints. This section helps us understand the diverse consumer experiences across the United States."),
              fluidRow(
                column(6,
                       div(style = "display: flex; flex-direction: column; justify-content: center;",
                           selectInput("selected_state", "Select a State:", choices = c("All States", sort(unique(merged_data$name))), selected = "All States"),
                       )
                )
              ),
              leafletOutput("mapPlot"),
              uiOutput("stateInsight"),  # New output for state-specific insights
              fluidRow(
                column(6,
                       div(style = "display: flex; flex-direction: column; justify-content: center;",
                           selectizeInput("compare_states", "Compare States:", choices = sort(unique(merged_data$name)), selected = NULL, multiple = TRUE, options = list(plugins = list('remove_button')))
                       )
                )
              ),
              plotlyOutput("comparisonPlot"),
              uiOutput("insightBlock")  # Insights for the bar chart comparison
          )
      ),
      div(class = "section", id = "month-over-month-section",
          div(class = "card",
              h2("Monthly change in complaints"),
              p("As we move forward, the focus shifts to the temporal dynamics of consumer complaints. The month-over-month analysis section allows us to see how the volume of complaints fluctuates over time, revealing any significant spikes or drops that may correlate with external events or internal changes within the bank. By adjusting the year range and selecting specific product categories, we can pinpoint periods that demand further investigation."),
              p(" "),
              div(style = "display: flex; flex-direction: column;",
                  div(style = "display: flex; align-items: flex-start;",
                      div(style = "flex: 1; margin-right: 10px;",
                          sliderInput("yearRange", "Select Year Range:", min = min_year, max = max_year, value = c(min_year, max_year), step = 1, sep = ""),
                          p(" "),
                          checkboxGroupInput("product_category", "Select Product Categories:", choices = unique(data$Product), selected = unique(data$Product)[1:5])
                      ),
                      div(style = "flex: 2;",
                          plotlyOutput("heatmapPlot", height = "400px")
                      )
                  ),
                  div(style = "display: flex; align-items: center; margin-top: 20px;",  # Changed align-items to center
                      div(style = "flex: 1; padding-right: 10px; box-sizing: border-box; text-align: center;",
                          p(" "),
                          p(" "),
                          h4("Top 10 Issues in Selected Product Categories"),
                          plotlyOutput("topIssuesPlot", height = "500px")
                      ),
                      div(style = "flex: 1; padding-left: 10px; box-sizing: border-box;",  # Added padding-left for alignment
                          uiOutput("dynamicInsights")
                      )
                  )
              )
          )
      ),
      div(class = "section", id = "complaints-trend-section",
          div(class = "card",
              h2("Complaints Trend"),
              p("Our journey concludes with a comprehensive trend analysis. Here, we look at the broader picture, examining the overall trends in consumer complaints. This section helps us identify long-term patterns and seasonal trends, providing a deep understanding of how consumer feedback evolves over the years. By analyzing submission methods and comparison metrics, we uncover the preferences and behaviors of consumers, which inform strategic decisions and improvements."),
              div(style = "display: flex; flex-direction: row;", # Changed flex-direction to row
                  div(style = "flex: 1; margin-right: 10px;",
                      uiOutput("yearInput"),
                      checkboxGroupInput("selected_methods_polar", "Select Submission Methods:", choices = unique(data$`Submitted via`), selected = unique(data$`Submitted via`)),
                      radioButtons("comparison_metric", "Select Comparison Metric:", choices = list("Monthly Complaints" = "monthly_complaints", "Yearly Complaints" = "yearly_complaints"))
                  ),
                  div(style = "flex: 2;",
                      plotlyOutput("complaintsChart", height = "500px") # Adjusted height for better layout
                  )
              ),
              div(style = "margin-top: 20px;", # Added space between chart and insights
                  uiOutput("dynamicInsightsText")
              )
          )
      ),
      
      # Defining the footer consisting the datasource 
      div(class = "footer",
          p("© Datasource: Financial Consumer Complaints (2017-2023), Kaggle")
      )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Observe changes in the selected state and update insights accordingly
  observeEvent(input$selected_state, {
    if (input$selected_state == "All States") {
      total_complaints <- nrow(data)
      first_complaint_date <- min(data$Date_received)
      last_complaint_date <- max(data$Date_received)
      days_diff <- as.numeric(difftime(last_complaint_date, first_complaint_date, units = "days"))
      avg_complaints_per_month <- round(total_complaints / (days_diff / 30), 2)
      
      national_avg_complaints <- round(mean(complaint_data$complaints), 2)
      state_ranking <- "N/A"
      
      # Create state insight text
      state_insight <- paste(
        "Across all states, there have been a total of", total_complaints, 
        "consumer complaints recorded from", format(first_complaint_date, "%B %d, %Y"), 
        "to", format(last_complaint_date, "%B %d, %Y"), 
        ". On average, this amounts to approximately", avg_complaints_per_month, 
        "complaints per month per state. The national average is", national_avg_complaints, 
        "complaints per state."
      )
      
      # Render the nationwide insights
      output$stateInsight <- renderUI({
        div(
          h4("Nationwide Insights"),
          p(state_insight)
        )
      })
    } else {
      selected_state_data <- data %>%
        filter(State_full == input$selected_state)
      
      total_complaints <- nrow(selected_state_data)
      first_complaint_date <- min(selected_state_data$Date_received)
      last_complaint_date <- max(selected_state_data$Date_received)
      days_diff <- as.numeric(difftime(last_complaint_date, first_complaint_date, units = "days"))
      avg_complaints_per_month <- round(total_complaints / (days_diff / 30), 2)
      
      national_avg_complaints <- round(mean(complaint_data$complaints), 2)
      state_ranking <- complaint_data %>%
        arrange(desc(complaints)) %>%
        mutate(ranking = row_number()) %>%
        filter(State_full == input$selected_state) %>%
        pull(ranking) # Calculate the ranking of the selected state
      
      # Create state-specific insight text
      state_insight <- paste(
        "In the state of", input$selected_state, ", there have been a number of ", 
        "consumer complaints recorded from", format(first_complaint_date, "%B %d, %Y"), 
        "to", format(last_complaint_date, "%B %d, %Y"), 
        ". On average, this amounts to approximately", avg_complaints_per_month, 
        "complaints per month. When compared to the national average of", national_avg_complaints, 
        "complaints per state, this indicates that", input$selected_state, "ranks", state_ranking, 
        "among all states. This suggests a relatively ", 
        ifelse(total_complaints > national_avg_complaints, "higher", "lower"), 
        " level of consumer engagement or issues reported in this state."
      )
      
      # Render the state-specific insights
      output$stateInsight <- renderUI({
        div(
          h4("State-Specific Insights"),
          p(state_insight)
        )
      })
    }
  })
  
  output$mapPlot <- renderLeaflet({
    req(input$selected_state)
    
    if (input$selected_state == "All States") {
      selected_data <- merged_data
    } else {
      selected_data <- merged_data[merged_data$name == input$selected_state, ]
    }
    
    pal <- colorNumeric(palette = "Blues", domain = merged_data$complaints)
    
    leaflet(data = selected_data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(complaints), 
        fillOpacity = 0.7, 
        color = "#BDBDC3", 
        weight = 1, 
        label = ~paste("State: ", name, "Complaints: ", complaints),
        highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)
      )
  })
  
  output$comparisonPlot <- renderPlotly({
    req(input$compare_states)
    
    comparison_data <- complaint_data %>%
      filter(State_full %in% input$compare_states)
    
    comparison_data$label <- paste("State: ", comparison_data$State_full, "<br>Complaints: ", comparison_data$complaints)
    
    p <- ggplot(comparison_data, aes(x = State_full, y = complaints, fill = State_full, text = label)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      labs(title = "Comparison of Complaints Across Selected States",
           x = "State",
           y = "Number of Complaints") +
      theme_minimal() +
      theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_brewer(palette = "Set3")
    
    ggplotly(p, tooltip = "text") %>% layout(width = "100%")
  })
  
  output$insightBlock <- renderUI({
    req(input$compare_states)
    
    comparison_data <- complaint_data %>%
      filter(State_full %in% input$compare_states)
    
    if (nrow(comparison_data) == 0) {
      return(div(class = "insight-text",
                 h4("Comparison Insights"),
                 p("No states selected for comparison.")))
    }
    
    total_complaints <- sum(comparison_data$complaints)
    max_state <- comparison_data %>%
      filter(complaints == max(complaints)) %>%
      pull(State_full)
    min_state <- comparison_data %>%
      filter(complaints == min(complaints)) %>%
      pull(State_full)
    
    avg_complaints <- round(mean(comparison_data$complaints), 2)
    max_complaints <- max(comparison_data$complaints)
    min_complaints <- min(comparison_data$complaints) 
    
    story <- paste0(
      "As we delve into the consumer complaints across the selected states, a few intriguing insights emerge. ",
      "In total, these states have amassed ", total_complaints, " complaints. ",
      "Among these states, ", max_state, " stands out with the highest number of complaints, tallying up to ", max_complaints, 
      ". This could indicate a significant level of dissatisfaction or a higher population engaging with the bank's services. ",
      "On the other hand, ", min_state, " has the least number of complaints, with only ", min_complaints, 
      ". This stark contrast raises questions about the varying levels of consumer experiences and interactions across states. ",
      "On average, the selected states have about ", avg_complaints, " complaints each, highlighting the diverse nature of consumer feedback. ",
      "Understanding these nuances can help in tailoring customer service strategies to better address regional concerns."
    )
    
    div(class = "insight-text",
        h5("Comparison Insights"),
        p(story)
    )
  })
  
  output$heatmapPlot <- renderPlotly({
    req(input$yearRange, input$product_category)
    
    filtered_data <- monthly_change_data %>%
      filter(Year >= input$yearRange[1] & Year <= input$yearRange[2], Product %in% input$product_category)
    
    p <- ggplot(filtered_data, aes(x = Month, y = Year, fill = monthly_change, text = paste("Product: ", Product, "<br>Change: ", round(monthly_change, 2), "%"))) +
      geom_tile(color = "white") +
      scale_fill_viridis_c(option = "C", na.value = "grey50", name = "Percent Change") +
      labs(x = "Month", y = "Year") +
      theme_minimal() +
      theme(legend.position = "right")
    
    ggplotly(p, tooltip = "text") %>% layout(width = "100%")
  })
  
  # Add the radial bar chart for top issues
  output$topIssuesPlot <- renderPlotly({
    top_issues <- data %>%
      filter(Product %in% input$product_category) %>%
      group_by(Issue) %>%
      summarise(issue_count = n()) %>%
      arrange(desc(issue_count)) %>%
      head(10)
    
    plot_ly(top_issues, labels = ~Issue, values = ~issue_count, type = 'pie', hole = 0.4,
            textinfo = 'percent', insidetextorientation = 'horizontal') %>%
      layout(
        showlegend = FALSE,
        margin = list(l = 20, r = 20, b = 20, t = 30, pad = 10),
        width = 600,
        height = 400
      )
  })
  
  
  output$dynamicInsights <- renderUI({
    req(input$yearRange, input$product_category)
    
    filtered_data <- monthly_change_data %>%
      filter(Year >= input$yearRange[1] & Year <= input$yearRange[2], Product %in% input$product_category)
    
    avg_change <- round(mean(filtered_data$monthly_change, na.rm = TRUE), 2)
    max_complaint_product <- filtered_data %>%
      group_by(Product) %>%
      summarise(total_complaints = sum(monthly_complaints, na.rm = TRUE)) %>%
      arrange(desc(total_complaints)) %>%
      slice(1) %>%
      pull(Product)
    
    additional_insights <- paste0(
      "The average month-over-month change in complaints for the selected product categories is ", avg_change, "%. ",
      "This value represents the average percentage change in the number of complaints from one month to the next. ",
      "A positive value indicates an overall increase in complaints over the period, while a negative value suggests a decrease. ",
      "The product category with the most complaints is ", max_complaint_product, ". ",
      "This category has consistently had the highest number of complaints over the selected time period. ",
      "Understanding which product category has the most complaints can help focus efforts on addressing the most prevalent issues."
    )
    
    div(
      p(additional_insights),
      p("Another interesting aspect to consider is the trend of complaints over the years. This can shed light on whether the issues are getting better or worse."),
      p("Analyzing the monthly change in complaints can help in identifying specific months where complaints spike, possibly due to seasonal factors or new product launches.")
    )
  })
  
  
  output$yearInput <- renderUI({
    if (input$comparison_metric == "monthly_complaints") {
      selectInput("selected_year_single", "Select Year:",
                  choices = sort(unique(data$Year)),
                  selected = max(data$Year))
    } else {
      sliderInput("selected_year", "Select Year Range:",
                  min = min_year,
                  max = max_year,
                  value = c(min_year, max_year),
                  step = 1,
                  sep = "")
    }
  })
  
  output$complaintsChart <- renderPlotly({
    req(input$selected_methods_polar, input$comparison_metric)
    
    if (input$comparison_metric == "monthly_complaints") {
      req(input$selected_year_single)
      
      filtered_data <- data %>%
        filter(Year == input$selected_year_single,
               `Submitted via` %in% input$selected_methods_polar) %>%
        group_by(Month = month(Date_received, label = TRUE, abbr = TRUE), `Submitted via`) %>%
        summarise(complaints = n(), .groups = 'drop') %>%
        mutate(Month = factor(Month, levels = month.abb))
      
      p <- ggplot(filtered_data, aes(x = Month, y = complaints, color = `Submitted via`, group = `Submitted via`, text = paste("Month:", Month, "<br>Complaints:", complaints))) +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        scale_color_brewer(palette = "Set2") +
        labs(title = "Monthly Complaints by Submission Method",
             x = "Month",
             y = "Number of Complaints") +
        theme_minimal()
      
    } else {
      req(input$selected_year)
      
      filtered_data <- data %>%
        filter(Year >= input$selected_year[1] & Year <= input$selected_year[2],
               `Submitted via` %in% input$selected_methods_polar) %>%
        group_by(Year, `Submitted via`) %>%
        summarise(complaints = n(), .groups = 'drop')
      
      p <- ggplot(filtered_data, aes(x = factor(Year), y = complaints, color = `Submitted via`, group = `Submitted via`, text = paste("Year:", Year, "<br>Complaints:", complaints))) +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        scale_color_brewer(palette = "Set2") +
        labs(title = "Yearly Complaints by Submission Method",
             x = "Year",
             y = "Number of Complaints") +
        theme_minimal()
    }
    
    ggplotly(p, tooltip = "text") %>%
      animation_opts(
        frame = 1000, 
        transition = 500, 
        easing = "cubic-in-out"
      ) %>% 
      layout(
        transition = list(
          duration = 500,
          easing = "cubic-in-out"
        ),
        frame = list(
          duration = 500,
          redraw = FALSE
        ),
        width = "100%"
      )
  })
  
  output$dynamicInsightsText <- renderUI({
    req(input$selected_methods_polar, input$comparison_metric)
    
    if (input$comparison_metric == "monthly_complaints") {
      req(input$selected_year_single)
      
      filtered_data <- data %>%
        filter(Year == input$selected_year_single,
               `Submitted via` %in% input$selected_methods_polar) %>%
        group_by(Month = month(Date_received, label = TRUE, abbr = TRUE), `Submitted via`) %>%
        summarise(complaints = n(), .groups = 'drop') %>%
        mutate(Month = factor(Month, levels = month.abb))
      
      total_complaints <- sum(filtered_data$complaints)
      method_complaints <- filtered_data %>%
        group_by(`Submitted via`) %>%
        summarise(total = sum(complaints))
      
      top_method <- method_complaints %>%
        arrange(desc(total)) %>%
        slice(1) %>%
        pull(`Submitted via`)
      
      top_method_complaints <- method_complaints %>%
        filter(`Submitted via` == top_method) %>%
        pull(total)
      
      insight_text <- paste(
        "<p style='font-family: \"Roboto\", sans-serif;'>",
        "In the year", input$selected_year_single, "the monthly complaints show distinct trends across different submission methods. ",
        "The '", top_method, "' method received the highest number of complaints throughout the year with", top_method_complaints, "complaints. ",
        "This trend suggests that many consumers prefer the", top_method, "method for lodging complaints. ",
        "Overall, the monthly complaints for the selected methods sum up to", total_complaints, "."
      )
      
    } else {
      req(input$selected_year)
      
      filtered_data <- data %>%
        filter(Year >= input$selected_year[1] & Year <= input$selected_year[2],
               `Submitted via` %in% input$selected_methods_polar) %>%
        group_by(Year, `Submitted via`) %>%
        summarise(complaints = n(), .groups = 'drop')
      
      total_complaints <- sum(filtered_data$complaints)
      method_complaints <- filtered_data %>%
        group_by(`Submitted via`) %>%
        summarise(total = sum(complaints))
      
      top_method <- method_complaints %>%
        arrange(desc(total)) %>%
        slice(1) %>%
        pull(`Submitted via`)
      
      top_method_complaints <- method_complaints %>%
        filter(`Submitted via` == top_method) %>%
        pull(total)
      
      insight_text <- paste(
        "<p style='font-family: \"Roboto\", sans-serif;'>",
        "Over the selected years from", input$selected_year[1], "to", input$selected_year[2], "the yearly complaints exhibit notable patterns. ",
        "The '", top_method, "' submission method received the highest number of complaints with", top_method_complaints, "complaints. ",
        "This trend indicates a persistent preference for the", top_method, "method among consumers. ",
        "Total complaints for this period amount to", total_complaints, "."
      )
    }
    
    HTML(insight_text)
  })
}


# Run the application 
shinyApp(ui = ui, server = server)

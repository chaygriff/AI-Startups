library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinythemes)
library(tidyverse)
library(lubridate)
library(ggpubr)
library(plotly)
library(DT)
library(leaflet)
library(formattable)
library(rsconnect)
library(priceR)
library(reactable)
library(scales)
library(mailtoR)
library(htmltools)

ai <- read.csv("Book5.csv")
ai <- as_tibble(ai)

ai <- ai[!is.na(ai$last_funding), ]
ai <- ai %>%
  select(company, industry, employees, Site, last_funding, last_funding_date, funding_round, date_founded, description, hq_location, Glassdoor, Votes, Reviews)


ai$last_funding <- currency(ai$last_funding, digits = 0L)

rsconnect::setAccountInfo(name='channingg', token='8D7D5575B5129DCE4235EF079980B76B', secret='DoVogGwHo20wKbaHpa9zMNLJgn8kPMYK/7ZC56FM')

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("AI Startups who raised capital in the last 90 days"),
  p("Updated: November 3, 2022"),
  fluidRow(
    tags$script(src = "https://kit.fontawesome.com/752075d5c8.js"),
  tags$div(
    tags$i(class = "fa-solid fa-paper-plane",
           style = "font-size: 4.5rem; color:#0098f8;",
           style = "padding:3rem;")),
    tags$span("")
  ),
  mailtoR(email = "chgriffi@adobe.com",
          text = "Click here to email us your feedback!"),
  use_mailtoR(),

  
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("series",
                  "Filter data by last funding round:",
                  c("All",
                    unique(ai$funding_round))),
                  br(),
      selectInput("location",
                  "Filter data by HQ location:",
                  c("All",
                  sort(unique(c(ai$hq_location))))),
      br(),
                  sliderInput("slider",
                              "Filter plot by employee count:",
                              min = min(ai$employees), 
                              max = max(ai$employees),
                              value = max(ai$employees), step = 10)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", 
                           plotlyOutput("distPlot", height = 700)),
                  tabPanel("About", 
                           br(),
                           p("*Funding represents the total amount of capital raised in company's most recent round."),
                           p("*Click arrow for more information about the company"),
                           reactableOutput("table")),
                  tabPanel("Donut",
                           plotlyOutput("pieChart", height = 700)),
                  tabPanel("Glassdoor",
                           br(),
                           p("*Hover over stars for precise rating."),
                           reactableOutput("glasstable"))
                  
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlotly({
    
    data  <- ai
    if (input$series != "All") {
      data <- data[data$funding_round == input$series,]
    }
    if (input$location != "All") {
      data <- data[data$hq_location == input$location,]
    }
 
    data %>% filter(employees <= input$slider) %>%
      ggplot(aes(reorder(company, -last_funding), last_funding, fill = last_funding, label = company, label2 = last_funding, label3 = industry, label4 = employees)) +
      geom_bar(stat = "identity")+
      ggtitle("Dollars raised in last funding round", subtitle =  paste0(input$series))+
      theme_bw()+
      theme(axis.text.y = element_text(size = 13, hjust = 1),
            axis.text.x = element_text(size = 7, angle = 60, hjust =1),
            plot.title = element_text(size = 24, face = "italic"),
            plot.subtitle = element_text(size = 14, face = "italic"),
            panel.grid.major.x = element_blank(),
            legend.position = "none")+
      scale_y_continuous(labels = scales::dollar_format())+
      xlab("")+
      ylab("")
    
    ggplotly(tooltip = c("label", "label2", "label3", "label4"))
    
  })
  
  output$table <- renderReactable({
    data <- ai
  
    if (input$series != "All") {
      data <- data[data$funding_round == input$series,]
    }
    if (input$location != "All") {
      data <- data[data$hq_location == input$location,]
    }
    
    data <-  data %>% 
      select(company, Site, employees, hq_location, description, industry, funding_round, last_funding) %>% 
      arrange(desc(employees))
    
    data <- data %>% rename(Company = company,
                            Employees = employees,
                            HQ = hq_location,
                            About = description,
                            Industry = industry,
                            Series = funding_round,
                            Funding = last_funding)                 
  
    orange_pal <- function(x) rgb(colorRamp(c("#D3F4FB", "#53DFD1"))(x), maxColorValue = 255)

    
    reactable(
      data,
      searchable = TRUE,
      striped = TRUE,
      highlight = TRUE,
      bordered = TRUE,
      theme = reactableTheme(
        borderColor = "#dfe2e5",
        stripedColor = "#f6f8fa",
        highlightColor = "#f0f5f9",
        cellPadding = "8px 12px",
        style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
        searchInputStyle = list(width = "100%")),
      defaultPageSize = 25,
      details = function(index, About) {
        # input:
        #   - index, the row index
        #   - name, the column name (optional)
        #
        # output:
        #   - content to render (e.g., an HTML tag or nested table), or NULL to hide details
        htmltools::div(
          reactable(data[index, 5])
        )
      },
      columns = list(
        Site = colDef(align = "center", cell = function(value) {
          htmltools::tags$a(href = value, target = "_blank", value)
        }),
        About = colDef(show = FALSE),   
        Employees = colDef(align = "center", minWidth = 80),   
        Series = colDef(align = "center"),
        Company = colDef(minWidth = 80,
                         align = "center",
                         cell = function(value, index) {
                           Industry <- data$Industry[index]
                           Industry <- if (!is.na(Industry)) Industry else "Unknown"
                           div(
                             div(style = "font-weight: 600", value),
                             div(style = "font-size: 1.2rem", Industry))
                           }),
        HQ = colDef(align = "center", minWidth = 120),
        Industry = colDef(show = FALSE),
        Funding = colDef(align = "center",
          format = colFormat(prefix = "$", separators = TRUE, digits = 0),
          minWidth = 80,
          style = function(value) {
            normalized <- (value - min(data$Funding)) / (max(data$Funding) - min(data$Funding))
            color <- orange_pal(normalized)
            list(background = color)
            }
          )
        )
      )
    
  })
  
  
  output$pieChart <- renderPlotly({
    
    data  <- ai
    if (input$series != "All") {
      data <- data[data$funding_round == input$series,]
    }
    if (input$location != "All") {
      data <- data[data$hq_location == input$location,]
    }
    
    data <- data %>% filter(employees <= input$slider) %>%
      group_by(industry) %>%
      summarize(total = n())
    fig <- data %>%
      plot_ly(labels = ~industry, values = ~total)
    fig <- fig %>% add_pie(hole = 0.6)
    fig <- fig %>% layout(title = "Industry Breakdown",  showlegend = F,
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  })
  
  output$glasstable <- renderReactable({
    data  <- ai
    data <- data %>% rename(Company = company)
    if (input$series != "All") {
      data <- data[data$funding_round == input$series,]
    }
    
    data <- data %>% select(Company, industry, Glassdoor, Votes, Reviews) %>%
      filter(Glassdoor != 'NA')
    
    rating_stars <- function(rating, max_rating = 5) {
      star_icon <- function(empty = FALSE) {
        tagAppendAttributes(shiny::icon("star"),
                            style = paste("color:", if (empty) "#edf0f2" else "orange"),
                            "aria-hidden" = "true"
        )
      }
      rounded_rating <- floor(rating + 0.5)  # always round up
      stars <- lapply(seq_len(max_rating), function(i) {
        if (i <= rounded_rating) star_icon() else star_icon(empty = TRUE)
      })
      label <- sprintf("%s out of %s stars", rating, max_rating)
      div(title = label, role = "img", stars)
    }
    
    reactable(
      data,
      searchable = TRUE,
      striped = TRUE,
      highlight = TRUE,
      bordered = TRUE,
      theme = reactableTheme(
        borderColor = "#dfe2e5",
        stripedColor = "#f6f8fa",
        highlightColor = "#f0f5f9",
        cellPadding = "8px 12px",
        style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
        searchInputStyle = list(width = "100%")),
      defaultPageSize = 25,
      columns = list(
        industry = colDef(align = "center", show = FALSE),
        Reviews = colDef(align = "center",
                      cell = function(value) {
          htmltools::tags$a(href = value, target = "_blank", value)
        }),
        Company = colDef(minWidth = 80,
                         align = "center",
                         cell = function(value, index) {
                           Industry <- data$industry[index]
                           Industry <- if (!is.na(Industry)) Industry else "Unknown"
                           div(
                             div(style = "font-weight: 600", value),
                             div(style = "font-size: 1.2rem", Industry))
                         }),
        Votes = colDef(align = "center"),
        Glassdoor = colDef(align = "center",
                           cell = function(value) rating_stars(value))
        )
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

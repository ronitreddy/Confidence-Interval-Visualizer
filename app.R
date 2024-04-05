# Load necessary libraries
library(shiny)
library(ggplot2)
library(bslib)

# Define UI
ui <- fluidPage(
  theme = bs_theme(),
  
  tags$head(
    tags$style(HTML("
      .top-padding {
        padding-top: 20px;
      }
      .title-and-toggle {
        display: flex;
        justify-content: space-between;
        align-items: center;
      }
      .dark-mode-toggle {
        text-align: right;
        padding-right: 25px;
      }
    "))
  ),
  
  div(class = "top-padding"),
  
  div(class = "title-and-toggle",
      div(titlePanel("Confidence Interval Visualizer (Means)"), class = "title"),
      div(input_dark_mode(id = "dark_mode", mode = "light"), class = "dark-mode-toggle")
  ),
  
  tagList(
    div(h4("App Created By: Ronit Reddy"), style = "padding-bottom: 20px;")
  ),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("distribution", "Distribution",
                  choices = c("Normal Distribution (n ≥ 30 or known σ)", "t-Distribution (n < 30 and unknown σ)"),
                  selected = "Normal Distribution (n ≥ 30 or known σ)"),
      numericInput("size", "Sample Size (n)", value = 30),
      numericInput("mean", "Sample Mean (x̄)", value = 0),
      numericInput("sd", "Standard Deviation (sample: s, population: σ)", value = 1),
      sliderInput("confidence", "Confidence Level", min = 0.90, max = 0.999, value = 0.95, step = 0.01),
      helpText(a(href = "https://github.com/ronitreddy/confidence_interval_visualizer", target = "_blank", "View code")),
    ),
    
    mainPanel(
      plotOutput("ciPlot"),
      div(
        verbatimTextOutput("ciText"),
        style = "text-align: center; padding-top: 50px;"
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  observeEvent(input$dark_mode, {
    if (input$dark_mode == "dark") {
      showNotification("Welcome to the dark side!", duration = 2, type = "default")
    } else {
      showNotification("Back to the light!", duration = 2, type = "default")
    }
  })
  
  calculate_error_margin <- reactive({
    distribution <- input$distribution
    size <- input$size
    sd <- input$sd
    confidence <- input$confidence
    
    if (distribution == "Normal Distribution (n ≥ 30 or known σ)") {
      z_score <- qnorm(confidence + (1 - confidence) / 2)
    } else if (distribution == "t-Distribution (n < 30 and unknown σ)") {
      z_score <- qt(confidence + (1 - confidence) / 2, df = size - 1)
    }
    
    z_score * sd / sqrt(size)
  })
  
  output$ciPlot <- renderPlot({
    mean <- input$mean
    error_margin <- calculate_error_margin()
    lower_bound <- mean - error_margin
    upper_bound <- mean + error_margin
    
    ggplot() +
      geom_point(aes(x = mean, y = 0), color = "#428BCA", size = 4) +
      geom_segment(aes(x = lower_bound, y = 0, xend = upper_bound, yend = 0), color = "#428BCA", size = 1) +
      geom_errorbarh(aes(xmin = lower_bound, xmax = upper_bound, y = 0), height = 0.1, color = "#428BCA", size = 1) +
      coord_cartesian(ylim = c(-1, 1)) +
      theme_minimal() +
      labs(title = "Confidence Interval", x = "", y = "") +
      theme(
        plot.title = element_text(
          size = 20, 
          hjust = 0.5, 
          margin = margin(t = 25, b = 25, unit = "pt")
        ),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)
      )
  })
  
  output$ciText <- renderText({
    mean <- input$mean
    error_margin <- calculate_error_margin()
    confidence <- input$confidence
    lower_bound <- mean - error_margin
    upper_bound <- mean + error_margin
    
    sprintf("With a sample mean of %s, we are %s%% confident the interval (%.3f, %.3f) captures the true population mean.", 
            mean, 
            confidence * 100, 
            lower_bound, 
            upper_bound)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
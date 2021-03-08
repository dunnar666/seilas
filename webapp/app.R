# Load packages
library(shiny)
library(shinythemes)
library(dplyr)
library(readr)

# Load data
seilas <- read_delim("seilas2020.csv",
                     ";", escape_double = FALSE, locale = locale(decimal_mark = ",",
                                                                 grouping_mark = ".", encoding = "ISO-8859-1"),
                     trim_ws = TRUE)


# Define UI
ui <- fluidPage(theme = shinytheme("lumen"),
                titlePanel("Anløpsdata 2020"),
                sidebarLayout(
                    sidebarPanel(

                        # Select type of trend to plot
                        selectInput(inputId = "type", label = strong("Trend index"),
                                    choices = unique(seilas$kommunenavn_ankomst),
                                    selected = "Kommune"),

                        # Select date range to be plotted
                        dateRangeInput("date", strong("Tidsperiode"), start = "2020-01-01", end = "2020-12-31",
                                       min = "2020-01-01", max = "2020-121-31"),

                        # Select whether to overlay smooth trend line
                        checkboxInput(inputId = "smoother", label = strong("Overlay smooth trend line"), value = FALSE),

                        # Display only if the smoother is checked
                        conditionalPanel(condition = "input.smoother == true",
                                         sliderInput(inputId = "f", label = "Smoother span:",
                                                     min = 0.01, max = 1, value = 0.67, step = 0.01,
                                                     animate = animationOptions(interval = 100)),
                                         HTML("Higher values give more smoothness.")
                        )
                    ),

                    # Output: Description, lineplot, and reference
                    mainPanel(
                        plotOutput(outputId = "lineplot", height = "300px"),
                        textOutput(outputId = "desc"),
                        tags$a(href = "https://www.kystdatahuset.no", "Source: Kystdatahuset", target = "_blank")
                    )
                )
)

# Define server function
server <- function(input, output) {

    # Subset data
    selected_trends <- reactive({
        req(input$date)
        validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
        validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
        trend_data %>%
            filter(
                type == input$type,
                date > as.POSIXct(input$date[1]) & date < as.POSIXct(input$date[2]
                ))
    })


    # Create scatterplot object the plotOutput function is expecting
    output$lineplot <- renderPlot({
        color = "#434343"
        par(mar = c(4, 4, 1, 1))
        plot(x = selected_trends()$date, y = selected_trends()$close, type = "l",
             xlab = "Dato", ylab = "Trend index", col = color, fg = color, col.lab = color, col.axis = color)
        # Display only if smoother is checked
        if(input$smoother){
            smooth_curve <- lowess(x = as.numeric(selected_trends()$date), y = selected_trends()$close, f = input$f)
            lines(smooth_curve, col = "#E6553A", lwd = 3)
        }
    })

    # Pull in description of trend
    output$desc <- renderText({
        paste("Indeksen er basert på seilaser utført i Norge i 2020.")
    })
}

# Create Shiny object
shinyApp(ui = ui, server = server)

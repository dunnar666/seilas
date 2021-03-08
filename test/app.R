# Load packages
library(shiny)
library(shinythemes)
library(dplyr)
library(readr)

# Load data
seilas2020 <- read_delim("seilas2020.csv",
                     ";", escape_double = FALSE, locale = locale(decimal_mark = ",",
                                                                 grouping_mark = ".", encoding = "ISO-8859-1"),
                     trim_ws = TRUE)

seilas2018 <- read_delim("seilas2018.csv",
                         ";", escape_double = FALSE, locale = locale(decimal_mark = ",",
                                                                     grouping_mark = ".", encoding = "ISO-8859-1"),
                         trim_ws = TRUE)

seilas2019 <- read_delim("seilas2019.csv",
                         ";", escape_double = FALSE, locale = locale(decimal_mark = ",",
                                                                     grouping_mark = ".", encoding = "ISO-8859-1"),
                         trim_ws = TRUE)

# Define UI for dataset viewer app ----
ui <- fluidPage(theme = shinytheme("sandstone"),

    # App title ----
    titlePanel("Seilaser i Norge"),

    # Sidebar layout with a input and output definitions ----
    sidebarLayout(

        # Sidebar panel for inputs ----
        sidebarPanel(

            # Input: Selector for choosing dataset ----
            selectInput(inputId = "dataset",
                        label = "Velg år:",
                        choices = c("seilas2018","seilas2019", "seilas2020")),

            # Input: Numeric entry for number of obs to view ----
            numericInput(inputId = "obs",
                         label = "Vis antall observasjoner:",
                         value = 10)
        ),

        # Main panel for displaying outputs ----
        mainPanel(

            # Output: Verbatim text for data summary ----
            verbatimTextOutput("summary"),

            # Output: HTML table with requested number of observations ----
            tableOutput("view"),
            textOutput(outputId = "desc"),
            tags$a(href = "https://www.kystdatahuset.no", "Kilde: Kystdatahuset", target = "_blank")

        )
    )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {

    # Return the requested dataset ----
    datasetInput <- reactive({
        switch(input$dataset,
               "seilas2018" = seilas2018,
               "seilas2019" = seilas2019,
               "seilas2020" = seilas2020)
    })


    # Show the first "n" observations ----
    output$view <- renderTable({
        head(datasetInput(), n = input$obs)
    })

    # Pull in description of trend
    output$desc <- renderText({
        paste("Indeksen er basert på seilaser utført i Norge i 2018, 2019 og 2020.")
})
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)

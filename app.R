library(shiny)
library(menzerath)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Menzerath-Altmann's law in text."),
    

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textAreaInput("text",
                          label = "Sample text to analyze",
                          value = "Greece* {which* is* the* most* beau*ti*ful* coun*try* +I* know* +}was* the* first* place* +we* vi*si*ted* in* Eu*ro*pe* +.",
                          rows = "10"),
            helpText("Delimiters should be a single character"),
            textInput("construct_delimiter",
                      label = "Construct delimiter",
                      value = "+",
                      placeholder = "+"),
            textInput("constituent_delimiter",
                      label = "Constituent delimiter",
                      value = " ",
                      placeholder = " "),
            textInput("subconstituent_delimiter",
                      label = "Subconstituent delimiter",
                      value = "*",
                      placeholder = "*"),
            textInput("discontinued_constituent_delimiter_begin",
                      label = "Discontinued constituent delimiter begin",
                      value = "{",
                      placeholder = "{"),
            textInput("discontinued_constituent_delimiter_end",
                      label = "Discontinued constituent delimiter end",
                      value = "}",
                      placeholder = "}"),

        ),
        

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),

        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        counts_df <- process_text(input$text,
                                  input$construct_delimiter,
                                  input$constituent_delimiter,
                                  input$subconstituent_delimiter,
                                  input$discontinued_constituent_delimiter_begin,
                                  input$discontinued_constituent_delimiter_end)
        counts_df$average_subconstituents = counts_df$subconstituents/counts_df$constituents
        mz <- menzerath(counts_df,x="constituents", y = "average_subconstituents")
        plot(mz, fit = TRUE)
    })

}

# Run the application 
shinyApp(ui = ui, server = server)

library(shiny)
library(menzerath)
library(gitlink)

equation_list = list("MAL"="$$y = ax^be^{-cx}$$",
                     "simplified_1"= "$$y = ae^{-cx}$$",
                     "simplified_2"= "$$y = ax^b$$",
                     "Milicka_1"= "$$L_{n-1} = a_nL_n^{-b_n}e^{c_nL_n}$$",
                     "Milicka_2"= "$$L_{n-1} = a_nL_n^{-b_n}$$",
                     "Milicka_4"= "$$L_{n-1} = a_n + \\frac{b_n}{L_n}$$",
                     "Milicka_8"= "$$L_{n-1} = a_n + \\frac{b_n}{L_n} + \\frac{c_n\\min(1,L_n-1)}{L_n}$$")

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textAreaInput("text",
                          label = "Sample text to analyze",
                          value = "Greece* {which* is* the* most* beau*ti*ful* coun*try* +I* know* +}was* the* first* place* +we* vi*si*ted* in* Eu*ro*pe* +.",
                          rows = "10"),
            p("Delimiters should be a single character"),
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
            selectInput("method",
                        "Fitting Method",
                        c("MAL",
                          "simplified_1",
                          "simplified_2",
                          "Milicka_1",
                          "Milicka_2"#, the remaining are yet unimplemented
                          # "Milicka_4",
                          # "Milicka_8"
                          ),
            )
        ),
        

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           p("Method for fitting:"),
           uiOutput("equation"),
           p("Fitted parameters:"),
           tableOutput("parameters"),
           a("https://sellisd.github.io/menzerath/", href="https://sellisd.github.io/menzerath/")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    mz_object <- reactive({
        counts_df <- process_text(input$text,
                                  input$construct_delimiter,
                                  input$constituent_delimiter,
                                  input$subconstituent_delimiter,
                                  input$discontinued_constituent_delimiter_begin,
                                  input$discontinued_constituent_delimiter_end)
        counts_df$average_subconstituents = counts_df$subconstituents/counts_df$constituents
        menzerath(counts_df,x="constituents", y = "average_subconstituents")}
    )

    output$distPlot <- renderPlot({
        mz <- mz_object()
        plot(mz, fit = TRUE, method=input$method)
    })
    output$equation <- renderUI(withMathJax(equation_list[[input$method]]))
    output$parameters <- renderTable({
        mz <- mz_object()
        fitted_mz <- fit(mz, method = input$method)
        get_parameters(fitted_mz)
    })
    

}

# Run the application 
shinyApp(ui = ui, server = server)

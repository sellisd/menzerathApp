library(shiny)
library(menzerath)
library(gitlink)
library(shinyFeedback)
library(readr)
library(glue)
library(gitlink)

equation_list = list("MAL"="$$y = ax^be^{-cx}$$",
                     "simplified_1"= "$$y = ae^{-cx}$$",
                     "simplified_2"= "$$y = ax^b$$",
                     "Milicka_1"= "$$L_{n-1} = a_nL_n^{-b_n}e^{c_nL_n}$$",
                     "Milicka_2"= "$$L_{n-1} = a_nL_n^{-b_n}$$",
                     "Milicka_4"= "$$L_{n-1} = a_n + \\frac{b_n}{L_n}$$",
                     "Milicka_8"= "$$L_{n-1} = a_n + \\frac{b_n}{L_n} + \\frac{c_n\\min(1,L_n-1)}{L_n}$$")

error_codes = list("1" = "Discontinued constituent delimiters are not balanced",
                   "2" = "Construct delimiter is not after a constituent and a subconstituent delimiter",
                   "3" = "Constituent delimiter is not after a subconstituent delimiter")

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = bslib::bs_theme(bootswatch = "pulse"),
    shinyFeedback::useShinyFeedback(),
    ribbon_css("https://github.com/sellisd/menzerathApp", position = "right", text = "Fork me on GitHub"),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            fileInput("upload", "Upload a text file to analyze"),
            textAreaInput("text",
                          label = "or type / paste text",
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
                          )),
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
server <- function(input, output, session) {
    #thematic::thematic_shiny()
    
    mz_object <- reactive({
        counts_df <- process_text(input$text,
                                  input$construct_delimiter,
                                  input$constituent_delimiter,
                                  input$subconstituent_delimiter,
                                  input$discontinued_constituent_delimiter_begin,
                                  input$discontinued_constituent_delimiter_end)
        is_valid <- !('Errors' %in% colnames(counts_df))
        shinyFeedback::feedbackDanger("text", !is_valid, glue("Error: {error_codes[counts_df$Errors]}"))
        req(is_valid)
        counts_df$average_subconstituents = counts_df$subconstituents/counts_df$constituents
        menzerath(counts_df,x="constituents", y = "average_subconstituents")}
    )

    output$distPlot <- renderPlot({
        mz <- mz_object()
        if(nobs(mz) == 0 ){
            plot(mz, fit = FALSE) + 
                ggplot2::xlim(0, 10) + 
                ggplot2::ylim(0, 10) +
                ggplot2::annotate("text", x = 5, y = 5, label = "No Data")
        }else{
          plot(mz, fit = TRUE, method=input$method)
        }
    })
    output$equation <- renderUI(withMathJax(equation_list[[input$method]]))
    output$parameters <- renderTable({
        validate_construct_delimiter()
        validate_constituent_delimiter()
        validate_subconstituent_delimiter()
        validate_discontinued_constituent_delimiter_begin()
        validate_discontinued_constituent_delimiter_end()
        mz <- mz_object()
        if(nobs(mz)==0){
            df <- data.frame()
        }else{
            fitted_mz <- fit(mz, method = input$method)
            df <- get_parameters(fitted_mz)
        }
        df
    })
    
    is_cpp_char <- function(x){
        is.character(x) && nchar(x) == 1 && length(x) == 1
    }

    # User feedback for each delimiter    
    validate_construct_delimiter <- reactive({
        is_char <- is_cpp_char(input$construct_delimiter)
        shinyFeedback::feedbackDanger("construct_delimiter", !is_char, "delimiter must be single character")
        req(is_char)
        input$construct_delimiter
    })
    validate_constituent_delimiter <- reactive({
        is_char <- is_cpp_char(input$constituent_delimiter)
        shinyFeedback::feedbackDanger("constituent_delimiter", !is_char, "delimiter must be single character")
        req(is_char)
        input$constituent_delimiter
    })
    validate_subconstituent_delimiter <- reactive({
        is_char <- is_cpp_char(input$subconstituent_delimiter)
        shinyFeedback::feedbackDanger("subconstituent_delimiter", !is_char, "delimiter must be single character")
        req(is_char)
        input$subconstituent_delimiter
    })
    validate_discontinued_constituent_delimiter_begin <- reactive({
        is_char <- is_cpp_char(input$discontinued_constituent_delimiter_begin)
        shinyFeedback::feedbackDanger("discontinued_constituent_delimiter_begin", !is_char, "delimiter must be single character")
        req(is_char)
        input$discontinued_constituent_delimiter_begin
    })
    validate_discontinued_constituent_delimiter_end <- reactive({
        is_char <- is_cpp_char(input$discontinued_constituent_delimiter_end)
        shinyFeedback::feedbackDanger("discontinued_constituent_delimiter_end", !is_char, "delimiter must be single character")
        req(is_char)
        input$discontinued_constituent_delimiter_end
    })
    
    # Upload text
    observe({
        req(input$upload)
        updateTextAreaInput(session, "text", value = read_file(input$upload$datapath))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

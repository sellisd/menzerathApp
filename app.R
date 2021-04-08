library(shiny)
library(menzerath)
library(gitlink)
library(shinyFeedback)
library(readr)
library(glue)
library(gitlink)
library(hyphenatr)


construct_delimiter <- " "
constituent_delimiter <- "-"
subconstituent_delimiter <- "/"
discontinued_constituent_delimiter_begin <- "{"
discontinued_constituent_delimiter_end <- "}"

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

graphemes <- c("p"="h","c"="k","q"="u","c"="h","s"="h","t"="h","n"="g")

merge_characters_list <- function(x){
    merged <- vector(mode="list", length = length(x))
    for(i in c(1:length(x))){
        merged[[i]] <- merge_characters(x[[i]])
    }
    merged
}

#' Merge consecutive characters
#'
#' @param x 
#'
#' @return array with merged characters
#' @export
#'
#' @examples
merge_characters <- function(x){
  #in a character array merge consecutive characters that form a single grapheme
  if(length(x)<=2){
      return(x)
  }
  to_merge <- numeric(0)
  previous <- x[1]
  for(i in c(2:length(x))){
      if( (x[i] == previous) ||
          ((previous %in% names(graphemes)) && (graphemes[previous] ==x[i])) ){
          to_merge <- c(to_merge,i)
          x[i-1] = paste0(x[i-1],x[i], sep="", collapse = "")
      }
      previous = x[i]
  }
  if(length(to_merge>0)){
      x <- x[-to_merge]
  }
  return(x)
}

collapse_words <- function(x){
    paste(sapply(x,paste,collapse="/"),collapse="/-")
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Menzerath's law in words, syllables and phonemes"),
    theme = bslib::bs_theme(bootswatch = "pulse"),
    shinyFeedback::useShinyFeedback(),
    ribbon_css("https://github.com/sellisd/menzerathApp/issues", position = "right", text = "Report a bug"),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            p(" annotate it with dashes (-) separating syllables and slashes (/) separating phonemes."),
            fileInput("upload", "Upload a text file to analyze"),
            textAreaInput("text",
                          label = "or type / paste text",
                          value = "G/r/ee/c/e/- wh/i/ch/- i/s/- th/e/- m/o/s/t/- b/e/au/-t/i/-f/u/l/- c/ou/n/-t/r/y/- I/- k/n/o/w/- w/a/s/- th/e/- f/i/r/s/t/- p/l/a/c/e/- w/e/- v/i/s/-i/t/e/d/- i/n/- E/u/r/o/p/e/.",
                          rows = "10",
                          resize = "none"),
            actionButton("autoannotate", "Auto-annotate"),
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
    observeEvent(input$autoannotate, {
        words <- strsplit(input$text, split = " ", fixed = TRUE)[[1]]
        tryCatch({
          hyphenated <- hyphenate(words, simplify = FALSE)
          graphemes_proxy <- sapply(hyphenated,strsplit, split="")
          graphemes_proxy <- sapply(graphemes_proxy, merge_characters_list)
          updateTextAreaInput(session, "text", value = paste(sapply(graphemes_proxy,collapse_words),collapse="/- "))
          },
          error = function(e){showNotification("This is an error.")})
        
    })
    mz_object <- reactive({
        counts_df <- process_text(input$text,
                                  construct_delimiter,
                                  constituent_delimiter,
                                  subconstituent_delimiter,
                                  discontinued_constituent_delimiter_begin,
                                  discontinued_constituent_delimiter_end)
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
    # Upload text
    observe({
        req(input$upload)
        updateTextAreaInput(session, "text", value = read_file(input$upload$datapath))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

require(shiny)
require(tm)
require(RColorBrewer)
require(tidyverse)
require(magrittr)
require(ggwordcloud)


scripture = sacred::king_james_version

malachi = scripture %>% 
    filter(book == "mal")%>% 
    select(text)

malachi.corpus = malachi %>% 
    tm::VectorSource() %>% 
    tm::VCorpus()

toSpace = content_transformer(function (x , pattern ) 
    gsub(pattern, " ", x))

malachi.corpus = malachi.corpus %>% 
    tm_map(toSpace, "/") %>%
    tm_map(toSpace, " ") %>%
    tm_map(toSpace, "\\|")

malachi.corpus =  malachi.corpus %>% 
    tm_map(FUN = content_transformer(tolower)) %>% # Convert the text to lower case
    tm_map(FUN = removeNumbers) %>% # Remove numbers
    tm_map(removeWords, stopwords("english")) %>% # Remove english common stopwords
    tm_map(removeWords, c("ye", "O", "unto", "yet", "thee", "wherein", "neither", "shall", 
                          "saith", "host", "will", "offer", "say")) %>%   # Remove words
    tm_map(removePunctuation) %>%   # Remove punctuations
    tm_map(stripWhitespace)  

# malachi.corpus %>% inspect()

malachi.corpus.tb=  malachi.corpus %>% 
    tm::TermDocumentMatrix(control = list(removeNumbers = TRUE,
                                          stopwords = TRUE,
                                          stemming = TRUE)) %>% 
    as.matrix() %>% as.data.frame() %>% 
    tibble::rownames_to_column() %>%
    dplyr::rename(word = 1, freq = 2) %>%
    dplyr::arrange(desc(freq)) %>%
    mutate(book = "Malachi")


## user interface
ui = fluidPage(
    sidebarLayout(
        sidebarPanel(width = 2,
            selectInput(inputId = "book", 
                        label = "Choose a Book", 
                        choices = list("Malachi", "Mathew", "John"), 
                        selected = "Malachi"),
            actionButton(inputId = "update", label = "Change"),
            hr(),
            sliderInput(inputId = "freq", label = "Miniumum Frequency", min = 0, max = 50, value = 25, step = 10),
            sliderInput(inputId = "word", label = "Number of Words", min = 0, max = 100, value = 20, step = 15)
        ),
        mainPanel(
            plotOutput(outputId = "plot", width = "100%", height = 400)
        )
    )
)

## server
server = function(input, output){
    
    # Define a reactive expression for the document term matrix
    terms <- reactive({
        # Change when the "update" button is pressed...
        input$update
        # ...but not for anything else
        isolate({
            withProgress({
                setProgress(message = "Processing corpus...")
                getTermMatrix(input$book)
            })
        })
    })
    
    output$plot = renderPlot({
        
        ggplot(data = malachi.corpus.tb %>% filter(book == input$book), 
               aes(label = word, size = freq, col = as.character(freq))) + 
            geom_text_wordcloud(rm_outside = TRUE, max_steps = 1,
                                grid_size = 1, eccentricity = .9)+
            scale_size_area(max_size = 20)+
            scale_color_brewer(palette = "Paired", direction = -1)+
            theme_void()
        
    })
    
}

## the application
shinyApp(ui = ui, server = server)




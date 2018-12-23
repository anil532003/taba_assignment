#---------------------------------------------------------------------#
#               k-means Clustering App                               #
#---------------------------------------------------------------------#

library("shiny")
if (!require(tm)) {install.packages("tm")}
if (!require(wordcloud)) {install.packages("wordcloud")}
if (!require(igraph)) {install.packages("igraph")}
if (!require(ggraph)) {install.packages("ggraph")}
library(tm) 
library(tidyverse)
library(tidytext)
library(wordcloud)
library(igraph)
library(ggraph)
library(udpipe)
library(textrank)
library(lattice)
library(igraph)
library(ggraph)
library(ggplot2)
library(wordcloud)
library(stringr)
campaigns_list = list("PROPN","NOUN", "ADJ")
# Define ui function
ui <- shinyUI(
  fluidPage(
    
    titlePanel("Coocurrance graph"),
    #textOutput("txt"),
    
    sidebarLayout( 
      
      sidebarPanel(  
        
        fileInput("file", "Upload data (Text file only)"),
        fileInput("langfile", "Please select the UDPIPE file (for language selection)"),
        
        checkboxGroupInput("checkgroup", label = h3("Please select POSTaggig"), 
                           choices = list("PROPN" = "PROPN", "ADJ" = "ADJ", "NOUN" = "NOUN", "VERB" = "VERB", "PRON" = "PRON"),
                           selected=campaigns_list)),
      
      mainPanel(
        
        tabsetPanel(type = "tabs",
                    
                    tabPanel("Overview",
                             h4(p("Data input")),
                             p("This app supports only text data file",align="justify"),
                             br(),
                             h4('How to use this App'),
                             p('To use this app, click on', 
                               span(strong("Upload data (text file with)")),
                               'and uppload the UDPIPE file')),
                    tabPanel("COG", 
                             plotOutput('plot1'))
                    
        ) # end of tabsetPanel
      )# end of main panel
    ) # end of sidebarLayout
  )  # end if fluidPage
) # end of UI



# Define Server function
server <- shinyServer(function(input, output, session) {
  options(shiny.maxRequestSize = 50*1024^2)
  Dataset <- reactive({
    if (is.null(input$file) || is.null(input$langfile)) { return(NULL) }
    else{
      windowsFonts(devanew=windowsFont("Devanagari new normal"))
      model = udpipe_load_model(input$langfile$datapath) 
      windowsFonts(devanew=windowsFont("Devanagari new normal"))
      Data <- readLines(input$file$datapath)
      windowsFonts(devanew=windowsFont("Devanagari new normal"))
      #cleantext =  textclean(Data)
      x <- udpipe_annotate(model, x = Data)
      x <- as.data.frame(x)
      return(x)
    }
  })
  #############################CLEAN FUNCTION BELOW####################################
  textclean = function(x,                    # x=text_corpus
                        remove_numbers=TRUE,        # whether to drop numbers? Default is TRUE  
                        remove_stopwords=TRUE)      # whether to drop stopwords? Default is TRUE
  { library(tm)
    x  =  gsub("<.*?>", " ", x)               # regex for removing HTML tags
    x  =  iconv(x, "latin1", "ASCII", sub="") # Keep only ASCII characters
    x  =  gsub("[^[:alnum:]]", " ", x)        # keep only alpha numeric 
    x  =  tolower(x)                          # convert to lower case characters
    if (remove_numbers) { x  =  removeNumbers(x)}    # removing numbers
    x  =  stripWhitespace(x)                  # removing white space
    x  =  gsub("^\\s+|\\s+$", "", x)          # remove leading and trailing white space. Note regex usage
    # evlauate condn
    if (remove_stopwords){
      # read std stopwords list from my git
      stpw1 = readLines('https://raw.githubusercontent.com/sudhir-voleti/basic-text-analysis-shinyapp/master/data/stopwords.txt')
      
      # tm package stop word list; tokenizer package has the same name function, hence 'tm::'
      stpw2 = tm::stopwords('english')      
      comn  = unique(c(stpw1, stpw2))         # Union of the two lists
      stopwords = unique(gsub("'"," ",comn))  # final stop word list after removing punctuation
      # removing stopwords created above
      x  =  removeWords(x,stopwords)           }  # if condn ends
    x  =  stripWhitespace(x)                  # removing white space
    # x  =  stemDocument(x)                   # can stem doc if needed. For Later.
    return(x) }  # func ends
  
  #########################CLEAN FUNCTION END HERE#######################################################
  
  output$plot1 = renderPlot({ 
    data_cooc <- cooccurrence(     # try `?cooccurrence` for parm options
      x = subset(Dataset(), upos %in% c(input$checkgroup)), 
      term = "lemma", 
      group = c("doc_id", "paragraph_id", "sentence_id")) 
    
    library(igraph)
    library(ggraph)
    library(ggplot2)
    
    wordnetwork <- head(data_cooc, 50)
    wordnetwork <- igraph::graph_from_data_frame(wordnetwork) # needs edgelist in first 2 colms.
    
    ggraph(wordnetwork, layout = "fr") +  
      
      geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "orange") +  
      geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
      
      theme_graph(base_family = "Arial Narrow") +  
      theme(legend.position = "none") +
      
      labs(title = "Cooccurrences within 3 words distance", subtitle = "Nouns & Adjective")
  })
  
  output$txt <- renderText({
      icons <- paste(input$checkgroup, collapse = ", ")
      return(icons)
    })
})

shinyApp(ui = ui, server = server)

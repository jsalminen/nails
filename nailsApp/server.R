
library(shiny)
library(knitr)
source("clean.R")
source("analyse.R")
source("topicmodel2.R")

# Load variable names
fieldtags <- read.csv("fieldtags.csv", header = T, sep = ";")

# Define server logic
shinyServer(function(input, output) {
    # Set max file size
    options(shiny.maxRequestSize=30*1024^2)
    
    observe({
        # Read input file
        inFile <- input$file1
        if (is.null(inFile))
            return(NULL)
        literature <- read.delim2(inFile$datapath, header = T,
                                  fileEncoding = "UTF-16", row.names = NULL,
                                  quote = "", stringsAsFactors=FALSE)
        # Clean data
        literature <- fix_columns(literature)
        literature <- add_id(literature)
        literature <- fix_variable_names(literature, fieldtags)
        literature <- fix_strings(literature)
        literature$Locations <- sapply(literature$AuthorAddress, get_location)
        
        # Add variable ReferenceString to literature.
        # Information of a record in same format as in CitedReferences
        literature$ReferenceString <- apply(literature, 1, makeRef)
        
        # Add column for core literature
        literature$CoreLiterature <- FALSE
        
        # Remove duplicates
        literature <- literature[!duplicated(literature[, "ReferenceString"]), ]
        
        # Preprocess data
        literatureByAuthor <- group_by_author(literature)
        literatureByKeywords <- group_by_keyword(literature)
        using_KeywordsPlus <- literatureByKeywords[[2]]
        literatureByKeywords <- literatureByKeywords[[1]]
        literatureByCategory <- group_by_category(literature)
        citationNetwork <- get_citation_network(literature)
        citationNodes <- citationNetwork[[1]]
        citationEdges <- citationNetwork[[2]]
        rm(citationNetwork)     # Free memory
        
        yearPlots <- plot_years(literature)
        authorPlots <- plot_authors(literatureByAuthor, literature)
        publicationPlots <- plot_publications(literature)
        keywordPlots <- plot_keywords(literature, literatureByKeywords,
                                      using_KeywordsPlus)
        
        citationData <- analyse_citations(citationEdges, citationNodes, literature)
        citationsLit <- citationData$citationsLit
        citationsLit <- citationsLit[with (citationsLit, order(-TimesCited)), ]
        citationsRef <- citationData$citationsRef
        citationsRef <- citationsRef[with (citationsRef, order(-InDegree)), ]
        rm(citationData)    # Free memory
        
        topicModel <- create_topicmodel(literature$Abstract)
        literature$TopicModelTopic <- topicModel$tfdDF$toptopic
        tw <- data.frame(topicModel$topwords)
        colnames(tw) <- gsub('X', 'Topic ', colnames(tw))
        kable(tw, col.names = colnames(tw))
        
        output$yearPlotAbs <- renderPlot(yearPlots$yearPlotAbs)
        output$yearPlotRel <- renderPlot(yearPlots$yearPlotRel)
        
        output$productiveAuthors <- renderPlot(authorPlots$productiveAuthors)
        output$citedAuthors <- renderPlot(authorPlots$citedAuthors)
        
        output$popularPubs <- renderPlot(publicationPlots$popularPubs)
        output$citedPubs <- renderPlot(publicationPlots$citedPubs)
        
        output$popularKeywords <- renderPlot(keywordPlots$popularKeywords)
        output$citedKeywords <- renderPlot(keywordPlots$citedKeywords)
        
        output$included <- renderTable(head(citationsLit[, c("Article", 
                                                             "InDegree", 
                                                             "TimesCited",
                                                             "PageRank")], 25),
                                       digits=6)
        output$notIncluded <- renderTable(head(citationsRef[, c("FullReference", 
                                                               "InDegree", 
                                                               "PageRank")], 25),
                                          digits=6)
        
        output$topics <- renderTable(tw)
    })
    

    
  
})

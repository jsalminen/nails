
require(shiny)
require(knitr)
source("clean.R", local = TRUE)
source("analyse.R", local = TRUE)
source("topicmodel2.R", local = TRUE)

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
        
        authors <- process_authors(literatureByAuthor, literature)
        publications <- process_publications(literature)
        keywords <- process_keywords(literature, literatureByKeywords,
                                      using_KeywordsPlus)
        
        citationData <- analyse_citations(citationEdges, citationNodes, literature)
        citationsLit <- citationData$citationsLit
        citationsLit <- citationsLit[with (citationsLit, order(-TimesCited)), ]
        citationsRef <- citationData$citationsRef
        citationsRef <- citationsRef[with (citationsRef, order(-InDegree)), ]
        rm(citationData)    # Free memory
        
        # Set up topic model
        TopicModel <- create_topicmodel(literature$Abstract)
        literature$TopicModelTopic <- TopicModel$tfdDF$toptopic
        tw <- data.frame(TopicModel$topwords)
        colnames(tw) <- gsub('X', 'Topic ', colnames(tw))
        
        output$yearPlotAbs <- renderPlot({plot_year_abs(literature, input$yearBins)})
        output$yearPlotRel <- renderPlot({plot_year_rel(literature)})
        
        output$productiveAuthors <- renderPlot({plot_authors_prod(authors, 
                                                                  input$nAuthors)})
        output$citedAuthors <- renderPlot({plot_authors_cited(authors, 
                                                              input$nAuthors)})
        
        output$popularPubs <- renderPlot({plot_publications_pop(publications, 
                                                                input$nPubs)})
        output$citedPubs <- renderPlot({plot_publications_cited(publications,
                                                                input$nPubs)})
        
        output$popularKeywords <- renderPlot({plot_keywords_pop(keywords, 
                                                                input$nKeywords)})
        output$citedKeywords <- renderPlot({plot_keywords_cited(keywords, 
                                                                input$nKeywords)})
        observe({
            if (input$showPapers == "Literature") {
                output$papers <- renderTable({create_lit_table(citationsLit,
                                                                 input$sortPapers,
                                                                 input$nPapers)},
                                             digits=6)
            }
            else if (input$showPapers == "References") {
                output$papers <- renderTable({create_ref_table(citationsRef,
                                                                 input$sortPapers,
                                                                 input$nPapers)},
                                             digits=6)
            }
        })
        
        output$topics <- renderTable(tw)
        output$LDAViz <- renderVis(TopicModel$json)
    })
    

    
  
})

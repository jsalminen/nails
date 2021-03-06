
require(shiny)
require(knitr)
require(plyr)
source("clean.R", local = TRUE)
source("analyse.R", local = TRUE)
source("topicmodel2.R", local = TRUE)

# Load variable names
fieldtags <- read.csv("fieldtags.csv", header = T, sep = ";")

# Define server logic
shinyServer(function(input, output) {
    # Set max file size
    options(shiny.maxRequestSize=50*1024^2)
    
    observe({
        # Read input file
        inFile <- input$file1
        if (is.null(inFile))
            return(NULL)

        literature <- ldply(inFile$datapath, read.delim2, header = TRUE,
                            fileEncoding="UTF-16", row.names = NULL,
                            quote="", stringsAsFactors = FALSE)
        
        # For debugging
        # literature <- read.delim2("swarm intelligence.txt", header = TRUE,
        #                           fileEncoding = "UTF-16", row.names = NULL,
        #                           quote="", stringsAsFactors = FALSE)
        
        observeEvent(input$run, {
            
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
            
            uploadText <- paste(nrow(literature), "papers loaded.")
            output$uploadedPapers <- renderText(uploadText)
            
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
            
            authorNodes <- get_author_nodes(authors)
            authorEdges <- get_author_edges(authors, literature)
            
            citationData <- analyse_citations(citationEdges, citationNodes, literature)
            citationsLit <- citationData$citationsLit
            citationsLit <- citationsLit[with (citationsLit, order(-TimesCited)), ]
            citationsRef <- citationData$citationsRef
            citationsRef <- citationsRef[with (citationsRef, order(-InDegree)), ]
            rm(citationData)    # Free memory
            
            # Set up topic model
            TopicModelR <- reactive({create_topicmodel(literature$Abstract, 
                                                              input$nTopics)})
            
            literature$TopicModelTopic <- TopicModelR()$tfdDF$toptopic    # Needed only for report
            # tw <- data.frame(TopicModel$topwords)      # Do in topicmodel2.R or output
            # colnames(tw) <- gsub('X', 'Topic ', colnames(tw))
            
            output$yearPlotAbs <- renderPlot({plot_year_abs(literature, input$colorScheme)})
            output$yearPlotRel <- renderPlot({plot_year_rel(literature, input$colorScheme)})
            
            output$productiveAuthors <- renderPlot({plot_authors_prod(authors, 
                                                                      input$colorScheme)})
            output$citedAuthors <- renderPlot({plot_authors_cited(authors, 
                                                                  input$colorScheme)})
            
            output$popularPubs <- renderPlot({plot_publications_pop(publications, 
                                                                    input$colorScheme)})
            output$citedPubs <- renderPlot({plot_publications_cited(publications,
                                                                    input$colorScheme)})
            
            output$popularKeywords <- renderPlot({plot_keywords_pop(keywords, 
                                                                    input$colorScheme)})
            output$citedKeywords <- renderPlot({plot_keywords_cited(keywords, 
                                                                    input$colorScheme)})
            
            authorNet <- graph_from_data_frame(d=authorEdges, vertices=authorNodes)
            netClusters <- clusters(authorNet, "weak")
            
            observe({
                min_size <- reactive({
                    get_min_size(max(netClusters$csize), input$min_size)
                })
                
                clustersToPlot <- which(netClusters$csize >= min_size())
                
                output$author_plots <- renderUI({
                    plot_output_list <- lapply(1:length(clustersToPlot), function(i) {
                        plotname <- paste("plot", i, sep="")
                        plotOutput(plotname)
                    })
                    do.call(tagList, plot_output_list)
                })
                
                for (i in 1:length(clustersToPlot)) {
                    local({
                        my_i <- i
                        plotname <- paste("plot", my_i, sep="")
                        subnet <- induced_subgraph(authorNet, 
                                                   V(authorNet)[netClusters$membership == 
                                                                    clustersToPlot[i]])
                        degrees <- igraph::degree(subnet)
                        l <- layout_nicely(subnet)
                        
                        output[[plotname]] <- renderPlot({
                            plot_author_cluster(subnet, degrees, 
                                                input$node_size, 
                                                input$text_size)
                        })
                    })
                }
            })
            
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
            
            output$topics <- renderTable(TopicModelR()$topwords)
            output$LDAViz <- renderVis(TopicModelR()$json)
            
            output$dlreport <- downloadHandler(
                filename = function() {
                    paste("report", sep = ".", switch(
                        input$format, PDF = "pdf", HTML = "html", Word = ".docx"
                    ))
                },
                content = function(file) {
                    src <- normalizePath("report.Rmd")
                    # temporarily switch to the temp dir, in case you do not have write
                    # permission to the current working directory
                    owd <- setwd(tempdir())
                    on.exit(setwd(owd))
                    file.copy(src, 'report.Rmd')
                    
                    library(rmarkdown)
                    out <- render('report.Rmd', switch(
                        input$format,
                        HTML = html_document())
                    )
                    file.rename(out, file)
                }
            )
            datasetInput <- reactive({
                switch (input$dataset,
                    "Literature" = literature,
                    "Citation Network Nodes" = citationNodes,
                    "Citation Network Edges" = citationEdges,
                    "Author Network Nodes" = authorNodes,
                    "Author Network Edges" = authorEdges)
            })
            
            output$table <- renderTable({
                datasetInput()
            })
            
            output$dldata <- downloadHandler(
                filename = function() {
                    paste(input$dataset, ".csv", sep = "")
                },
                content = function(file) {
                    write.csv2(datasetInput(), file, row.names = FALSE)
                }
            )
        })
    })
})

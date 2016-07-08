#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    # Application title
    titlePanel("Literature Review"),
  
    # Sidebar with file input
    sidebarLayout(
        sidebarPanel(
            conditionalPanel(condition="input.conditionedPanels == 1",
                             helpText("Data manipulation"),
                             fileInput("file1",
                                       "Upload file(s)",
                                       multiple=TRUE,
                                       accept=c("text/tab-separated-values"))),
            conditionalPanel(condition="input.conditionedPanels == 2",
                             helpText("Plot panel"),
                             sliderInput("yearBins", "Bins", 10, 100, 30, step=1),
                             sliderInput("nAuthors", "Authors", 5, 30, 25, step=5),
                             sliderInput("nPubs", "Publications", 5, 30, 25, step=5),
                             sliderInput("nKeywords", "Keywords", 5, 30, 25, step=5)),
            conditionalPanel(condition="input.conditionedPanels == 3",
                             helpText("Paper panel"),
                             selectInput("showPapers", "Papers",
                                            choices=list("Literature",
                                                         "References")),
                             selectInput("sortPapers", "Sort by", 
                                         choices=list("TimesCited",
                                                      "InDegree",
                                                      "PageRank")),
                             sliderInput("nPapers", "Papers", 5, 100, 25, step=5)),
            conditionalPanel(condition="input.conditionedPanels == 4",
                             helpText("Topic model panel"))
            ),
    
        # Main panel displaying data table
        mainPanel(
            tabsetPanel(type="tabs", id="conditionedPanels",
                        tabPanel("Data manipulation", value=1),
                        tabPanel("Plots", value=2,
                                 plotOutput("yearPlotAbs"),
                                 plotOutput("yearPlotRel"),
                                 plotOutput("productiveAuthors"),
                                 plotOutput("citedAuthors"),
                                 plotOutput("popularPubs"),
                                 plotOutput("citedPubs"),
                                 plotOutput("popularKeywords"),
                                 plotOutput("citedKeywords")),
                        tabPanel("Important papers", value=3,
                                 tableOutput("papers")),
                        tabPanel("Topic model", value=4,
                                 tableOutput("topics")))
            )
        )
    ))

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
                             helpText("Plot panel")),
            conditionalPanel(condition="input.conditionedPanels == 3",
                             helpText("Paper panel")),
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
                                 tableOutput("included"),
                                 tableOutput("notIncluded")),
                        tabPanel("Topic model", value=4,
                                 tableOutput("topics")))
            )
        )
    ))

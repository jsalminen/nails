library(ggplot2)
library(splitstackshape)
library(igraph)
library(knitr)

# Set ggplot theme
theme_set(theme_minimal(15))

# Load yearly publication data
years <- read.table("analyze.csv", sep = ";", header = T)

plot_year_abs <- function(df, bins) {
    yearPlotAbs <- ggplot(df, aes(YearPublished)) +
        geom_histogram(bins = bins, fill = "darkgreen") +
        ggtitle("Year published") +
        xlab("Year") +
        ylab("Article count")
    return(yearPlotAbs)
}

plot_year_rel <- function(df) {
    # Calculate relative publication counts
    yearTable <- table(df$YearPublished)    # Tabulate publication years
    yearDF <- as.data.frame(yearTable)              # Turn to dataframe
    names(yearDF) <- c("Year", "Freq")              # Fix column names
    
    # Merge to dataframe of total publication numbers (years)
    yearDF <- merge(yearDF, years, by.x = "Year", by.y = "Year")
    yearDF$Year <- as.numeric(as.character(yearDF$Year))    # factor to numeric
    # Calculate published articles per total articles by year
    yearDF$Fraction <- yearDF$Freq / yearDF$Records
    
    yearPlotRel <- ggplot(yearDF, aes(Year, Fraction, group = 1)) +
        geom_line(color = "darkgreen") +
        xlab("Year") +
        ylab("Fraction of all publications") +
        ggtitle("Relative publication volume")
    
    return(yearPlotRel)
}

process_authors <- function(literatureByAuthor, literature) {
    # Calculating total number of citations for each author
    citationSums <- aggregate(literatureByAuthor$TimesCited,
                              by = list(AuthorFullName = toupper(literatureByAuthor$AuthorFullName)),
                              FUN = sum, na.rm = T)
    
    # Fixing column names
    names(citationSums) <- c("AuthorFullName", "TotalTimesCited")
    # Crating new data frame to plot citations by author
    
    # Extract author names
    authorNames <- unlist(strsplit(literature$AuthorFullName, ";"))
    # Remove apostrophes
    authorNames <- gsub("'", "", authorNames)
    # Count author name frequencies
    authors <- table(authorNames)
    # Transform to a data frame
    authors <- as.data.frame(authors)
    # Merge with data frame containing the total times citated by each author
    authors <- merge(authors, citationSums, by.x = "authorNames",
                     by.y = "AuthorFullName" )
    # Fix column name
    names(authors)[1] <- "AuthorFullName"
    # Sort the table by total times sited, decreasing order
    authors <- authors[with (authors, order(-TotalTimesCited)), ]
    
    return(authors)
}

plot_authors_prod <- function(authors, n=25) {
    # Sort authors by number of articles, extract top 25,
    # and reorder factors for plotting
    authors <- authors[with (authors, order(-Freq)), ]
    authorsPop <- head(authors, n)
    authorsPop <- transform(authorsPop, AuthorFullName = reorder(AuthorFullName, Freq))
    
    productiveAuthors <- ggplot(authorsPop, aes(AuthorFullName, Freq)) +
        geom_bar(stat = "identity", fill = "blue") +
        coord_flip() +
        ggtitle("Productive authors") +
        xlab("Author") +
        ylab("Number of articles")
    
    return(productiveAuthors)
} 

plot_authors_cited <- function(authors, n=25) {
    # Reorder AuthorFullName factor according to TotalTimesCited (decreasing order)
    authors <- transform(authors,
                         AuthorFullName = reorder(AuthorFullName,
                                                  TotalTimesCited))
    
    citedAuthors <- ggplot(head(authors, n), aes(AuthorFullName, TotalTimesCited)) +
        geom_bar(stat = "identity", fill = "blue") +
        coord_flip() +
        ggtitle("Most cited authors") +
        xlab("Author") + ylab("Total times cited")
    
    return(citedAuthors)
}

process_publications <- function(literature) {
    # Calculating total citations for each publication.
    # Functionality same as for the authors, see above.
    
    citationSums <- aggregate(literature$TimesCited,
                              by = list(PublicationName= literature$PublicationName),
                              FUN = sum, na.rm = T)
    names(citationSums) <- c("PublicationName", "PublicationTotalCitations")
    citationSums <- citationSums[with (citationSums, order(-PublicationTotalCitations)), ]
    
    publications <- sort(table(literature$PublicationName), decreasing = T)
    publications <- as.data.frame(publications)
    names(publications) <- c("PublicationName", "Count")
    publications <- merge(publications, citationSums, by.x="PublicationName",
                          by.y="PublicationName")
    publications$PublicationName <- strtrim(publications$PublicationName, 50)
    publications <- transform(publications, PublicationName = reorder(PublicationName, Count))
 
    return(publications)   
}

plot_publications_pop <- function(publications, n=25) {
    popularPubs <- ggplot(head(publications, n), aes(PublicationName, Count)) +
        geom_bar(stat = "identity", fill = "orange") +
        coord_flip() +
        theme(legend.position = "none") +
        ggtitle("Most popular publications") +
        xlab("Publication") +
        ylab("Article count")
    
    return(popularPubs)
}
    
plot_publications_cited <- function(publications, n=25){
    publications <- transform(publications,
                              PublicationName = reorder(PublicationName,
                                                        PublicationTotalCitations))
    
    citedPubs <- ggplot(head(publications, n), aes(PublicationName, PublicationTotalCitations)) +
        geom_bar(stat = "identity", fill = "orange") +
        coord_flip() +
        theme(legend.position = "none") +
        ggtitle("Most cited publications") +
        xlab("Publication") + ylab("Total times cited")
    
    return(citedPubs)
}

process_keywords <- function(literature, literatureByKeywords, using_KeywordsPlus) {
    # Calculating total citations for each keyword
    # Functionality same as for the authors, see above.
    
    # Sometimes AuthorKeywords column is empty.
    # Following if-else hack prevents crashing in those situations,
    # either by using KeywordsPlus column or skipping keyword analysis.
    if (using_KeywordsPlus == TRUE) {
        # Change to use column names, not the number!!!
        names(literature)[c(21, 22)] <- c("AuthorKeywordsTemp", "AuthorKeywords")
    }
    
    if (nrow(literatureByKeywords) == 0) {
        return(NULL)
    } 
    else {
        # Below functionality same as above for important authors.
        keywordCitationSum <- aggregate(literatureByKeywords$TimesCited,
                                        by = list(AuthorKeywords =
                                                      literatureByKeywords$AuthorKeywords), 
                                        FUN = sum,
                                        na.rm = T)
        names(keywordCitationSum) <- c("AuthorKeywords", "TotalTimesCited")
        
        keywords <- unlist(strsplit(literature$AuthorKeywords, ";"))
        keywords <- trim(keywords)
        keywords <- table(keywords)
        keywords <- as.data.frame(keywords)
        names(keywords) <- c("AuthorKeywords", "Freq")
        
        keywords <- merge(keywords, keywordCitationSum, by = "AuthorKeywords")
        
        keywords <- keywords[with (keywords, order(-Freq)), ]
        keywords <- transform(keywords, AuthorKeywords =
                                     reorder(AuthorKeywords, Freq))
        
        # Change the column names back to original ones
        # Change to use column names, not the numbers!!!
        names(literature)[c(21, 22)] <- c("AuthorKeywords", "KeywordsPlus")
       
        return(keywords)
    }
}

plot_keywords_pop <- function(keywords, n=25) {
    popularKeywords <- ggplot(head(keywords, n), aes(AuthorKeywords, Freq)) +
        geom_bar(stat = "identity", fill = "purple") +
        coord_flip() +
        ggtitle("Popular keywords") +
        xlab("Keyword") +
        ylab("Number of occurences")
    
    return(popularKeywords)
}
    
plot_keywords_cited <- function(keywords, n=25) {
    keywords <- keywords[with (keywords, order(-TotalTimesCited)), ]
    keywords <- transform(keywords, AuthorKeywords =
                              reorder(AuthorKeywords, TotalTimesCited))
    citedKeywords <- ggplot(head(keywords, n), aes(AuthorKeywords, TotalTimesCited)) +
        geom_bar(stat = "identity", fill = "purple") +
        coord_flip()  +
        ggtitle("Most cited keywords") +
        xlab("Keyword") + ylab("Total times cited")
    
    return(citedKeywords)
}

analyse_citations <- function(citationEdges, citationNodes, literature) {
    # Create igraph
    citationGraph <- graph.data.frame(citationEdges, vertices = citationNodes)
    # Calculate PageRanks
    citationNodes$PageRank <- page.rank(citationGraph)$vector
    # Calculate in-degrees
    citationNodes$InDegree <- degree(citationGraph, mode = "in")
    
    # Extract the articles included in the data set and articles not included
    # in the dataset
    citationsLit <- citationNodes[citationNodes$Origin == "literature", ]
    citationsRef <- citationNodes[citationNodes$Origin == "reference", ]
    # Merge with selected columns in literature data frame
    citationsLit <- merge(citationsLit,
                          literature[, c("ReferenceString",
                                         "DocumentTitle")],
                          by.x = "FullReference", by.y = "ReferenceString")
    # Create article strings (document title, reference information and abstract
    # separated by "|")
    citationsLit$Article <- paste(toupper(citationsLit$DocumentTitle), " | ",
                                  citationsLit$FullReference, " | ",
                                  citationsLit$Abstract)
    
    # Trim FullReference to 100 characters
    citationsLit$FullReference <- strtrim(citationsLit$FullReference, 100)
    citationsRef$FullReference <- strtrim(citationsRef$FullReference, 100)
    
    return(list(citationsLit=citationsLit, citationsRef=citationsRef))
}

create_topicmodel <- function(abstracts) {
    # Do topic modeling on abstracts using the lda libraries (adding them as a new column)
    source("topicmodel.R", chdir = T)
    
    # Add top topic to main document
    literature$TopicModelTopic <- tfdDF$toptopic
}

# Support functions for nails app
require(splitstackshape)
require(reshape2)
require(stringr)
suppressPackageStartupMessages(library(plyr))

# Remove leading and trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

fix_columns <- function(df) {
    data.names <- names(df)[2:length(names(df))]
    df <- df[, 1:(ncol(df) - 1)]
    names(df) <- data.names
    return(df)
}

add_id <- function(df) {
    id <- c(1:nrow(df))
    ids <- data.frame(id = id)
    df = cbind(ids, df)
    return(df)
}

fix_variable_names <- function(df, fieldtags) {
    tags <- names(df)       # Extract column names
    # Match column names (acronyms) with full column names
    fields <- as.character(fieldtags$field[match(tags, fieldtags$tag)])
    fields[is.na(fields)] <- tags[is.na(fields)]     # Throws warnings but seems to be working
    fields <- gsub(" ", "", fields)         # Remove spaces
    
    # Change literature column names and fix weird names
    names(df) <- fields
    names(df)[names(df) == "KeywordsPlus\xfc\xbe\x8e\x86\x84\xbc"] <- "KeywordsPlus"
    names(df)[names(df) == "PublicationType(conference,book,journal,bookinseries,orpatent)"] <- "PublicationType"
    names(df)[names(df) == "29-CharacterSourceAbbreviation"] <- "SourceAbbreviation"
    names(df)[names(df) == "DigitalObjectIdentifier(DOI)" ] <- "DOI"
    return(df)
}

fix_strings <- function(df) {
    df$AuthorFullName <- toupper(df$AuthorFullName)
    df$AuthorFullName <- gsub("'", "", df$AuthorFullName)
    df$AuthorFullName <- gsub('"', "", df$AuthorFullName)
    
    df$AuthorKeywords <- tolower(df$AuthorKeywords)
    df$AuthorKeywords <- gsub("'", "", df$AuthorKeywords)
    df$AuthorKeywords <- gsub('"', "", df$AuthorKeywords)
    
    df$KeywordsPlus <- tolower(df$KeywordsPlus)
    df$KeywordsPlus <- gsub("'", "", df$KeywordsPlus)
    df$KeywordsPlus <- gsub('"', "", df$KeywordsPlus)
    
    df$YearPublished <- as.numeric(df$YearPublished)
    
    df$DocumentTitle <- gsub("'", "", df$DocumentTitle)
    df$DocumentTitle <- gsub('"', "", df$DocumentTitle)
    
    df$SubjectCategory <- tolower(df$SubjectCategory)
    df$SubjectCategory <- gsub("'", "", df$SubjectCategory)
    df$SubjectCategory <- gsub('"', "", df$SubjectCategory)
    
    df$CitedReferences <- gsub("'", "", df$CitedReferences)
    df$CitedReferences <- gsub('"', "", df$CitedReferences)
    df$CitedReferences <- toupper(df$CitedReferences)
    df$CitedReferences <- gsub("DOI DOI", "DOI", df$CitedReferences)
    df$TimesCited <- as.numeric(df$TimesCited)
    
    df$DOI <- toupper(df$DOI)
    
    df$YearPublished <- as.numeric(df$YearPublished)
    df$TimesCited <- as.numeric(df$TimesCited)
    return(df)
}

# Helper function for extracting countries and cities from AuthorAddress
get_location <- function(x) {
    country <- NA
    city <- NA
    if (x != "") {
        x <- gsub("\\[.*?\\]", "", x)
        x <- unlist(strsplit(x, ";"))
        x <- x[x != " "]
        cities <- sapply(x, function(x) tail(unlist(strsplit(x, ",")), 2))
        city <- apply(cities, 2, function(x) gsub(".*[0-9]+ ", "", x[1]))
        city <- sapply(city, trim)
        #   country <- gsub(" ", "", cities[2, ])
        country <- sapply(cities[2, ], trim)
        return(paste(paste(city, country, sep = ","), collapse = ";"))
    }
    else {
        return(NA)
    }
}

# Helper function to construct strings
makeRef <- function(x) {
    refstring <- getName(x)
    if (!is.na(x["YearPublished"])) {
        refstring <- paste(refstring, x["YearPublished"], sep = ", ")
    }
    if (x["SourceAbbreviation"] != "") {
        refstring <- paste(refstring, x["SourceAbbreviation"], sep = ", ")
    }
    if (!is.na(x["Volume"])) {
        refstring <- paste(refstring, ", V", x["Volume"], sep = "")
    }
    if (!is.na(x["BeginningPage"])) {
        refstring <- paste(refstring, ", P", x["BeginningPage"], sep = "")
    }
    if (x["DOI"] != "") {
        refstring <- paste(refstring, ", DOI ", x["DOI"], sep = "")
    }
    return(refstring)
}

# Helper function to extract the name of first author
getName <- function(x) {
    name = NA
    try( {
        names <- unlist(strsplit(x["AuthorFullName"], ";"))
        names <- names[1]
        names <- unlist(strsplit(names, " "))
        name <- names[1]
        name <- gsub(",", "", name)
        if (length(names) > 1) {
            name <- paste(name, substring(names[2], 1, 1))
        }
        if (length(names) > 2) {
            name <- paste(name, substring(names[3], 1, 1), sep = "")
        }
    } )
    return(name)
}

# Topic modeling

# Create a new data frame, where each author is in a separate row
group_by_author <- function(df) {
    # Subset data
    byAuthor = subset(df, select = c("AuthorFullName", "id"))
    
    # Remove NAs
    byAuthor <- byAuthor[!is.na(byAuthor$AuthorFullName),]
    
    # Create data frame: AuthorFullName split by ";", each name on a new row,
    # id copied to new rows
    byAuthor <- cSplit(byAuthor, splitCols = "AuthorFullName",
                       sep = ";", direction = "long")
    
    # Removing rows with NA as author name created in previous step
    byAuthor <- byAuthor[!is.na(byAuthor$AuthorFullName),]
    
    # Drop weird extra column created in previous step
    byAuthor <- subset(byAuthor, select = c("id", "AuthorFullName"))
    
    # Merge the rest of the data by id
    byAuthor <- merge(byAuthor, subset(df, select = -c(AuthorFullName)),
                      by = "id")
    
    return(byAuthor)
}

# Create a new data frame, where each keyword is in a separate row.
# Same functionality as with the author names, see above.

group_by_keyword <- function(df) {
    byKeywords <- subset(df, select = c("AuthorKeywords", "id"))
    byKeywords <- byKeywords[!is.na(byKeywords$AuthorKeywords),]
    byKeywords <- byKeywords[byKeywords$AuthorKeywords != "", ]
    using_KeywordsPlus = FALSE
    
    if (nrow(byKeywords) == 0) {
        byKeywords <- subset(df, select = c("KeywordsPlus", "id"))
        names(byKeywords)[1] <- "AuthorKeywords"
        byKeywords <- byKeywords[!is.na(byKeywords$AuthorKeywords),]
        byKeywords <- byKeywords[byKeywords$AuthorKeywords != "", ]
        using_KeywordsPlus = TRUE
    }
    
    if (nrow(byKeywords) > 0) {
        byKeywords <- cSplit(byKeywords, splitCols = "AuthorKeywords",
                             sep = ";", direction = "long")
        byKeywords <- byKeywords[!is.na(byKeywords$AuthorKeywords),]
        byKeywords <- subset(byKeywords, select = c("id", "AuthorKeywords"))
        byKeywords <- merge(byKeywords, 
                            subset(df, select = -c(AuthorKeywords)),
                            by = "id")
    }
    return(list(byKeywords, using_KeywordsPlus))
}

# Create new data frame, where each subject category is in a separate row.
# Same functionality as with the author names, see above.
group_by_category <- function(df) {
    byCategory <- subset(df, select = c("SubjectCategory", "id"))
    
    byCategory <- byCategory[!is.na(byCategory$SubjectCategory),]
    byCategory <- byCategory[byCategory$SubjectCategory != "", ]
    byCategory <- cSplit(byCategory, splitCols = "SubjectCategory",
                         sep = ";", direction = "long")
    byCategory <- byCategory[!is.na(byCategory$SubjectCategory),]
    byCategory <- subset(byCategory, select = c("id", "SubjectCategory"))
    byCategory <- merge(byCategory,
                        subset(df, select = -c(SubjectCategory)),
                        by = "id")
    byCategory$SubjectCategory <- trim(byCategory$SubjectCategory)
    return(byCategory)
}

# Citation network data

# Helper function to extract DOIs
getDOIs <- function(x) {
    if (length(x) == 2) {
        return(x[2])
    } else {
        return(NA)
    }
}

# Helper function to extract years
getYear <- function(x) {
    year = NA
    if (length(x) > 1) {
        year = as.numeric(x[2])
    }
    return(year)
}

get_citation_network <- function(df) {
    # Create a new data frame, where each cited reference is in a separate row
    
    # Create data frame: CitedReferences split by ";", each reference on a new row,
    # id copied to new rows
    referencelist <- strsplit(df$CitedReferences, ";")
    reflengths <- sapply(referencelist, length)
    id <- rep(df$id, reflengths)
    
    # Extract DOIs from references
    referencelist <- unlist(referencelist)
    referencelist <- trim(referencelist)
    references <- strsplit(referencelist, " DOI ")
    references <- sapply(references, getDOIs)
    
    # Extract publication years of references
    refYear <- strsplit(referencelist, ",")
    refYear <- sapply(refYear, getYear)
    
    # Create data frame with references and ids and merge literature to it
    referencedf <- data.frame(Reference = references, id = id,
                              FullReference = referencelist)
    referencedf <- merge(referencedf, df, by = "id")
    
    # Create data frame of nodes from references
    citationNodes <- data.frame(Id = referencedf$Reference,
                                YearPublished = refYear,
                                FullReference = referencelist,
                                id = NA,
                                PublicationType = NA,
                                AuthorFullName = NA,
                                DocumentTitle = NA,
                                PublicationName = NA,
                                BookSeriesTitle = NA,
                                Language = NA,
                                DocumentType = NA,
                                ConferenceTitle = NA,
                                ConferenceDate = NA,
                                ConferenceLocation = NA,
                                ConferenceSponsors = NA,
                                AuthorKeywords = NA,
                                SubjectCategory = NA,
                                TimesCited = NA,
                                Abstract = NA,
                                DOI = NA)
    citationNodes$Id <- as.character(citationNodes$Id)
    citationNodes$FullReference <- as.character(citationNodes$FullReference)
    citationNodes$Id[is.na(citationNodes$Id)] <- citationNodes$FullReference[
        is.na(citationNodes$Id)]
    citationNodes$Origin <- rep("reference", nrow(citationNodes))
    
    # Create data frame of nodes from literature records
    literatureNodes <- data.frame(Id = df$DOI,
                                  YearPublished = df$YearPublished,
                                  FullReference = df$ReferenceString)
    literatureNodes <- subset(df, select = c(DOI,
                                             YearPublished,
                                             ReferenceString,
                                             id,
                                             PublicationType,
                                             AuthorFullName,
                                             DocumentTitle,
                                             PublicationName,
                                             BookSeriesTitle,
                                             Language,
                                             DocumentType,
                                             ConferenceTitle,
                                             ConferenceDate,
                                             ConferenceLocation,
                                             ConferenceSponsors,
                                             AuthorKeywords,
                                             SubjectCategory,
                                             TimesCited,
                                             Abstract,
                                             DOI))
    names(literatureNodes)[c(1:3, 20)] <- c("Id", "YearPublished", "FullReference",
                                            "DOI")
    
    literatureNodes$Id <- as.character(literatureNodes$Id)
    literatureNodes$FullReference <- as.character(literatureNodes$FullReference)
    literatureNodes$Id[literatureNodes$Id == ""] <- literatureNodes$FullReference[
        literatureNodes$Id == ""]
    literatureNodes$Origin <- rep("literature", nrow(literatureNodes))
    
    # Remove reference nodes that appear also in literature data
    citationNodes <- citationNodes[!(citationNodes$Id %in% literatureNodes$Id), ]
    
    # Merge node data frames, remove NAs and duplicates, add Label column
    citationNodes <- rbind(citationNodes, literatureNodes)
    citationNodes <- citationNodes[!is.na(citationNodes$Id), ]
    citationNodes$YearPublished[is.na(citationNodes$YearPublished)] <- ""
    citationNodes <- citationNodes[!duplicated(citationNodes[, "Id"]), ]
    citationNodes$Label <- citationNodes$Id
    
    # Create citations edge table
    referencedf$ReferenceString <- as.character(referencedf$ReferenceString)
    referencedf$FullReference <- as.character(referencedf$FullReference)
    
    # Create table
    # (Weird way to handle missing DOIs as Source!! Consider revising!)
    citationEdges <- data.frame(Source = referencedf$DOI,
                                Target = referencedf$Reference,
                                id = referencedf$id,
                                YearPublished = referencedf$YearPublished,
                                DocumentTitle = referencedf$DocumentTitle)
    citationEdges$Source <- as.character(citationEdges$Source)
    citationEdges$Target <- as.character(citationEdges$Target)
    
    # Fixing Source, where DOI was missing
    noSource <- citationEdges$Source == ""
    noTarget <- is.na(citationEdges$Target)
    citationEdges$Source[noSource] <- referencedf$ReferenceString[noSource]
    citationEdges$Target[noTarget] <- referencedf$FullReference[noTarget]
    
    return(list(citationNodes, citationEdges))
}

# Author network

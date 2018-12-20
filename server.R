# server

library(tidyverse)
library(forcats)
library(reshape2)

library(gapminder)

library(shiny)
library(plotly)
library(leaflet)
library(dygraphs)
library(DT)
library(dendextend)
library(ggplot2)
library(matrixStats)
library(crosstalk)
library(wordcloud)
library(tm)
source("https://raw.githubusercontent.com/mateyneykov/315_code_data/master/code/geom_mosaic.R")
library(rsconnect)
library(RColorBrewer)



function(input, output) {

  color_palette <- palette(rainbow(20))
  get_colors <- function(x, palette = color_palette) palette[match(x, unique(x))]

  ##############################################################################
  #  Table output
  ##############################################################################

  output$table <- DT::renderDataTable(
    biopics
  )

  ##############################################################################
  # Basic ggplot, no interactivity
  ##############################################################################

  output$wordcloud <- renderPlot({
    docs <- Corpus(VectorSource(biopics$title))

    # Convert the text to lower case
    docs <- tm_map(docs, content_transformer(tolower))
    # Remove numbers
    docs <- tm_map(docs, removeNumbers)
    # Remove english common stopwords
    docs <- tm_map(docs, removeWords, stopwords("english"))
    # Remove your own stop word
    # specify your stopwords as a character vector
    docs <- tm_map(docs, removeWords, c("blabla1", "blabla2"))
    # Remove punctuations
    docs <- tm_map(docs, removePunctuation)
    # Eliminate extra white spaces
    docs <- tm_map(docs, stripWhitespace)

    dtm <- TermDocumentMatrix(docs)
    m <- as.matrix(dtm)
    v <- sort(rowSums(m),decreasing=TRUE)
    d <- data.frame(word = names(v),freq=v)

    set.seed(1234)

    p <- wordcloud(words = d$word, freq = d$freq, min.freq = 1,
                   max.words=input$word_adjust, random.order=FALSE, rot.per=0.35,
                   colors=brewer.pal(8, "Dark2"))

    return(p)
  })

  ##############################################################################
  # Traditional Shiny and mosaic plot
  ##############################################################################

  output$mosaic <- renderPlot({
    facet1 <- input$mosaic1
    facet2 <- input$mosaic2
    if (facet1 == "Race") {
      if (facet2 == "Race") {
        p <- ggplot(biopics, aes(x = subject_race, y = subject_race)) +
          geom_mosaic() +
          mosaic_legend() +
          labs(x = "Subject Race",
               y = "Subject Race",
               title = "Subject Race vs Subject Race") +
          theme(axis.text.x = element_text(angle = 90))
      } else if (facet2 == "Gender") {
        p <- ggplot(biopics, aes(x = subject_race, y = subject_sex)) +
          geom_mosaic() +
          mosaic_legend() +
          labs(x = "Subject Race",
               y = "Gender",
               title = "Subject Race vs Subject Gender") +
          theme(axis.text.x = element_text(angle = 90))
      } else if (facet2 == "Type of Subject") {
        p <- ggplot(biopics, aes(x = subject_race, y = type_of_subject)) +
          geom_mosaic() +
          mosaic_legend() +
          labs(x = "Subject Race",
               y = "Type of Subject",
               title = "Race vs Type of Subject") +
          theme(axis.text.x = element_text(angle = 90))
      }
    } else if (facet1 == "Gender") {
      if (facet2 == "Race") {
        p <- ggplot(biopics, aes(x = subject_sex, y = subject_race)) +
          geom_mosaic() +
          mosaic_legend() +
          labs(x = "Subject Gender",
               y = "Subject Race",
               title = "Subject Gender vs Subject Race") +
          theme(axis.text.x = element_text(angle = 90))
      } else if (facet2 == "Gender") {
        p <- ggplot(biopics, aes(x = subject_sex, y = subject_sex)) +
          geom_mosaic() +
          mosaic_legend() +
          labs(x = "Subject Gender",
               y = "Subject Gender",
               title = "Subject Gender vs Subject Gender") +
          theme(axis.text.x = element_text(angle = 90))
      } else if (facet2 == "Type of Subject") {
        p <- ggplot(biopics, aes(x = subject_sex, y = type_of_subject)) +
          geom_mosaic() +
          mosaic_legend() +
          labs(x = "Subject Gender",
               y = "Type of Subject",
               title = "Subject Gender vs Type of Subject") +
          theme(axis.text.x = element_text(angle = 90))
      }
    } else {
      if (facet2 == "Race") {
        p <- ggplot(biopics, aes(x = type_of_subject, y = subject_race)) +
          geom_mosaic() +
          mosaic_legend() +
          labs(x = "Type of Subject",
               y = "Subject Race",
               title = "Type of Subject vs Subject Race") +
          theme(axis.text.x = element_text(angle = 90))
      } else if (facet2 == "Gender") {
        p <- ggplot(biopics, aes(x = type_of_subject, y = subject_sex)) +
          geom_mosaic() +
          mosaic_legend() +
          labs(x = "Type of Subject",
               y = "Subject Gender",
               title = "Type of Subject vs Subject Gender") +
          theme(axis.text.x = element_text(angle = 90))
      } else if (facet2 == "Type of Subject") {
        p <- ggplot(biopics, aes(x = type_of_subject, y = type_of_subject)) +
          geom_mosaic() +
          mosaic_legend() +
          labs(x = "Type of Subject",
               y = "Type of Subject",
               title = "Type of Subject vs Type of Subject") +
          theme(axis.text.x = element_text(angle = 90))
      }
    }
    return(p)
  })

  ##############################################################################
  # Plotly and Histogram
  ##############################################################################

  output$histogram <- renderPlotly({
    graph <- ggplot(data = biopics,
                    aes(x = year_release,
                        fill = country,
                        text = paste("Country: ",
                                     fill,
                                     "\nNumber of films:",
                                     ..count..))) +
      geom_histogram(binwidth = as.numeric(input$binwidth)) +
      labs(title = "Biopics by Year and Country",
           x = "Year",
           y = "Number of Films",
           fill = "Country") +
      scale_fill_brewer(palette = "YlOrBr")

    p <- ggplotly(graph, tooltip = c("text"))
    return(p)
  })

  ############################################################################
  # Plotly and TimeSeries
  ############################################################################

  output$timeseries <- renderDygraph({
    year_totals <- biopics %>%
      group_by(year_release) %>%
      filter(box_office > 0) %>%
      summarize(averages = sum(as.numeric(box_office))/n(),
                totals = sum(as.numeric(box_office)),
                n_films = n())

    year_totals$inflation_mult <- c(17.52, 16.77, 15.15, 13.19, 12.90, 10.92,
                                    9.39, 8.51, 7.83, 7.72, 7.60, 7.38, 6.61,
                                    6.23, 5.90, 5.52, 5.08, 3.47, 3.06, 2.72,
                                    2.50, 2.41, 2.32, 2.23, 2.15, 2.12, 2.03,
                                    1.95, 1.86, 1.75, 1.70, 1.65, 1.61, 1.57,
                                    1.53, 1.48, 1.46, 1.43, 1.40, 1.35, 1.33,
                                    1.30, 1.27, 1.23, 1.19, 1.16, 1.12, 1.12,
                                    1.09, 1.07, 1.04, 1.02, 1.01, 1)

    year_totals$avg_inf <- year_totals$averages * year_totals$inflation_mult
    year_totals$totals_inf <- year_totals$totals * year_totals$inflation_mult

    graph <- dygraph(data = year_totals,
            main = "Box Office Earnings of Biopics (adjusted for inflation)",
            ylab = "USD (Millions)") %>%
      dySeries("totals_inf", "Gross total") %>%
      dySeries("avg_inf", "Average") %>%
      dySeries("n_films", "Number of films") %>%
      dyOptions(stackedGraph = FALSE, drawPoints = TRUE,
                pointSize = 2) %>%
      dyRangeSelector(height = 20) %>%
      dyAxis("x", drawGrid = FALSE) %>%
      dyLegend(show = "follow")

    return(graph)
  })

  ############################################################################
  # Plotly with Map
  ############################################################################

  output$plotly_plot_playable <- renderPlotly({
    need <- c("box_office", "country", "year_release", "title")
    sub_map <- biopics[need]
    length(sub_map)
    test <- sub_map[grepl("/",sub_map$country),]
    single <-sub_map[!grepl("/",sub_map$country),]
    test <- test[rep(1:nrow(test), each = 2),]

    for (i in seq(1, nrow(test), 2)){
      character <- as.character(test[i,"country"])
      s <- strsplit(character,"[/]")
      d <- (s[[1]])
      one <- d[1]
      two <- d[2]
      test[i,"country"] <- one
      test[i+1,"country"] <- two
    }

    sub_map <- rbind(single,test)
    sub_map <- sub_map[complete.cases(sub_map),]
    sub_map <- unique(sub_map)
    title_df <- sub_map %>% group_by(country) %>% filter(box_office == max(box_office))
    title <- title_df$title
    title <- title[c(2,3,1)]
    print((title))

    gb <- aggregate(sub_map, list(sub_map$country), mean)
    gb$code <- c("CAN","GBR","USA")
    gb$title <- title

    l <- list(color = toRGB("grey"), width = 0.5)

    plot_geo(gb) %>%
      add_trace(
        z = ~box_office, color = ~box_office, colors = 'Blues',
        text = paste("Highest Grossing Movie:",title),
        locations = ~code, marker = list(line = l)) %>%
      colorbar(title = 'Box Office Earnings (USD millions)', tickprefix = '$') %>%
      layout(title = 'Average Box Office Earning by Country')
  })

  ############################################################################
  # Boxplot
  ############################################################################

  output$boxplot <- renderPlot({
    selected <- input$Types
    sfacet <- input$facet
    data_sub <- subset(biopics, type_of_subject %in% selected)
    data_sub$subject_sex <- factor(data_sub$subject_sex)
    levels(data_sub$subject_sex)
    levels(data_sub$subject_sex) <- c("Female (Subject)", "Male (Subject)")

    if (sfacet == "Gender")
      ggplot(data_sub,aes(x = reorder(type_of_subject, box_office,
                                      FUN = median), y=box_office)) +
      geom_boxplot(aes(color=subject_race)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      facet_grid(. ~ subject_sex, margins = TRUE) +
      labs(title ="Boxplot of Box Office Earnings by Movie Biography Type",
           x = "Movie Biography Type", y = "Box Office Earnings", color = "Subject Race")
    else
      ggplot(data_sub,aes(x = reorder(type_of_subject, box_office,
                                      FUN = median), y=box_office)) +
      geom_boxplot(aes(color=subject_sex)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      facet_grid(. ~ subject_race, margins = TRUE) +
      labs(title ="Boxplot of Box Office Earnings by Movie Biography Type",
           x = "Movie Biography Type",y = "Box Office Earnings" ,color = "Gender")
  })


  ############################################################################
  # Stacked Bar Chart
  ############################################################################

  output$stacked_bar <-
    renderPlotly({
      plotly_subject <- ggplot(biopics, aes(x = type_of_subject, fill = subject_race)) +
        geom_bar() +
        theme(axis.text.x = element_text(angle = 45)) +
        labs(x = "Type of Subject", fill = "Frequency") +
        ggtitle("Biopics' Subjects' Professions and Race")
      p_plotly <- ggplotly(plotly_subject)
      return(p_plotly)
    })

  ############################################################################
  # Dendogram
  ############################################################################

  output$dendo <- renderPlot({
    color_palette <- palette(rainbow(20))
    get_colors <- function(x, palette = color_palette) palette[match(x, unique(x))]
    ##Directors by race
    biopics <- biopics %>% filter(!is.na(director))
    biopics <- biopics %>% filter(!is.na(subject_race))
    biopics <- biopics %>% filter()


    biopics_new <- biopics %>% dplyr :: select(subject_race, director, subject, type_of_subject, lead_actor_actress)

    biopics_new$subject_race = as.numeric(as.factor(biopics$subject_race))
    biopics_new$director = as.numeric(as.factor(biopics$director))
    biopics_new$subject = as.numeric(as.factor(biopics$subject))
    biopics_new$type_of_subject = as.numeric(as.factor(biopics$type_of_subject))
    biopics_new$lead_actor_actress = as.numeric(as.factor(biopics$lead_actor_actress))


    bio_cont <- biopics_new %>% select(c("subject_race", "director", "subject", "type_of_subject", "lead_actor_actress"))
    bio_cont_scale <- bio_cont %>% scale()
    dist_bio <- bio_cont_scale %>% dist()

    hc_bio_complete <- hclust(dist_bio, method = "complete")

    bio_dend <- as.dendrogram(hc_bio_complete)
    plot(bio_dend, leaflab = "none")


    dend <- bio_cont %>% scale %>% dist %>% hclust %>% as.dendrogram
    value <- input$subject_info
    if (value == "Subject Race") {value <- biopics$subject_race}
    else if(value == "Subject") {value <- biopics$subject}
    else {value <- biopics$type_of_subject}

    dend %>% set("labels", biopics$director, order_value = F) %>%
      set("labels_col", get_colors(value), order_value = F) %>%
      set("labels_cex", 0.1) %>%
      set("branches_lwd", 0.1) %>%
      ggplot(horiz = T)
  })

}

# ui.R

library(tidyverse)
library(forcats)
library(reshape2)

library(gapminder)

library(shiny)
library(plotly)
library(leaflet)
library(dygraphs)
library(DT)
library(ggplot2)
library(matrixStats)
library(shinydashboard)

source("https://raw.githubusercontent.com/mateyneykov/315_code_data/master/code/geom_mosaic.R")

library(crosstalk)
library(rsconnect)

dashboardPage(
  dashboardHeader(title = "Group 20 Biopics Project"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data", tabName = "data", icon = icon("table")),
      menuItem("WordCloud", tabName = "wordcloud", icon = icon("cloud")),
      menuItem("Mosaic", tabName = "mosaic", icon = icon("palette")),
      menuItem("Histogram", tabName = "histogram", icon = icon("chart-bar")),
      menuItem("Time Series", tabName = "time_series", icon = icon("clock")),
      menuItem("Map", tabName = "map", icon = icon("globe-americas")),
      menuItem("Boxplot", tabName = "boxplot", icon = icon("chart-bar")),
      menuItem("Stacked Bar Chart", tabName = "stacked_bar_chart", icon = icon("chart-bar")),
      menuItem("Dendogram", tabName = "dendogram", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "data",
                class = "active",
                bootstrapPage(
                  DT::dataTableOutput('table'))
      ),
      tabItem(tabName = "wordcloud",
              class = "active",
              bootstrapPage(
                sliderInput(inputId = "word_adjust",
                            label = "Word Frequency Adjustment",
                            min = 1, max = 200, value = 1,
                            step = 20),
                plotOutput("wordcloud", height = 500, width = 800)
              )
      ),
      tabItem(tabName = "mosaic",
                class = "active",
                bootstrapPage(
                  selectInput(input = "mosaic1",
                              label = "Select X axis Variable",
                              c("Gender","Race", "Type of Subject")),
                  selectInput(input = "mosaic2",
                              label = "Select Y Axis Variable",
                              c("Race","Gender", "Type of Subject")),
                  plotOutput(outputId = "mosaic", height = 500, width = 800)
                )
      ),
      tabItem(tabName = "histogram",
              class = "active",
              bootstrapPage(
                selectInput(inputId = "binwidth",
                            label = "Number of years per bin (approx.):",
                            choices = c(1, 5, 10, 20),
                            selected = 5),
                plotlyOutput(outputId = "histogram")
              )
      ),
      tabItem(tabName = "time_series",
              class = "active",
              bootstrapPage(
                dygraphOutput(outputId = "timeseries")
              )
      ),
      tabItem(tabName = "map",
              class = "active",
              bootstrapPage(
                plotlyOutput(outputId = "plotly_plot_playable",
                             height = 600,
                             width = 900)
              )
      ),
      tabItem(tabName = "boxplot",
              class = "active",
              bootstrapPage(
                selectInput("Types","Select Type of Movie Biography",
                            c("Criminal", "Other", "Athlete", "Musician",
                              "Author", "Academic", "Historical", "Actress",
                              "Journalist", "World leader", "Military", "Comedian",
                              "Actor", "Medicine", "Artist", "Activist", "Politician",
                              "Military / activist", "Singer",	"Author (poet)",
                              "Teacher", "Government", "Actress / activist",
                              "Media", "Athlete / military", "Academic (Philosopher)"),
                            selectize = TRUE, multiple = TRUE, selected = "Criminal"),
                selectInput("facet","Select Facet",
                            c("Gender","Race"),
                            selectize = TRUE, multiple = FALSE, selected = "Gender"),
                plotOutput(outputId = "boxplot",
                           height = 600,
                           width = 900)
              )
      ),
      tabItem(tabName = "stacked_bar_chart",
              class = "active",
              bootstrapPage(
                plotlyOutput(outputId = "stacked_bar",
                             height = 600,
                             width = 900)
              )
      ),
      tabItem(tabName = "dendogram",
              class = "active",
              bootstrapPage(
                selectInput(inputId = "subject_info",
                            label = "About Subject",
                            choices = c("Subject Race", "Subject", "Type of Subject"),
                            selected = "subject_race"),
                plotOutput(outputId = "dendo", height = "500px",
                           width = "700px")
              )
      )
    )
  )
)
# navbarPage(
#   title = "Group 20 Biopic Movie Analysis",
#
#   ##############################################################################
#   #  Table output
#   ##############################################################################
#
#   tabPanel("Data",
#            DT::dataTableOutput('table')
#   ),
#
#   ##############################################################################
#   # Basic wordcloud, no interactivity
#   ##############################################################################
#
#   tabPanel("Traditional Wordcloud",
#            sliderInput(inputId = "word_adjust",
#                        label = "Word Frequency Adjustment",
#                        min = 1, max = 200, value = 1,
#                        step = 20),
#            plotOutput("wordcloud", height = 500, width = 800)
#   ),
#
#   ##############################################################################
#   # Traditional Shiny and mosaic
#   ##############################################################################
#
#   tabPanel("Mosaic Plot",
#            selectInput(input = "mosaic1",
#                        label = "Select X axis Variable",
#                        c("Gender","Race", "Type of Subject")),
#            selectInput(input = "mosaic2",
#                        label = "Select Y Axis Variable",
#                        c("Race","Gender", "Type of Subject")),
#            plotOutput(outputId = "mosaic", height = 500, width = 800)
#
#   ),
#
#   ##############################################################################
#   # Traditional Shiny and ggplot
#   ##############################################################################
#
#   tabPanel("Histogram Plotly",
#            selectInput(inputId = "binwidth",
#                        label = "Number of years per bin (approx.):",
#                        choices = c(1, 5, 10, 20),
#                        selected = 5),
#
#            plotlyOutput(outputId = "histogram")
#   ),
#
#   ############################################################################
#   # Plotly and Shiny
#   ############################################################################
#
#   tabPanel("Time Series Plotly",
#            dygraphOutput(outputId = "timeseries")
#   ),
#   ############################################################################
#   # Playable Plotly
#   ############################################################################
#
#   tabPanel("Plotly of Map",
#            plotlyOutput(outputId = "plotly_plot_playable",
#                         height = 600,
#                         width = 900)
#   ),
#
#   ############################################################################
#   # Playable Plotly
#   ############################################################################
#
#   tabPanel("Boxplot",
#            selectInput("Types","Select Type of Movie Biography",
#                        c("Criminal", "Other", "Athlete", "Musician",
#                          "Author", "Academic", "Historical", "Actress",
#                          "Journalist", "World leader", "Military", "Comedian",
#                          "Actor", "Medicine", "Artist", "Activist", "Politician",
#                          "Military / activist", "Singer",	"Author (poet)",
#                          "Teacher", "Government", "Actress / activist",
#                          "Media", "Athlete / military", "Academic (Philosopher)"),
#                        selectize = TRUE, multiple = TRUE, selected = "Criminal"),
#            selectInput("facet","Select Facet",
#                        c("Gender","Race"),
#                        selectize = TRUE, multiple = FALSE, selected = "Gender"),
#            plotOutput(outputId = "boxplot",
#                         height = 600,
#                         width = 900)
#   ),
#
#   ############################################################################
#   # Stacked Bar Chart
#   ############################################################################
#
#   tabPanel("Stacked Bar Chart",
#            plotlyOutput(outputId = "stacked_bar",
#                         height = 600,
#                         width = 900)
#   ),
#
#   ############################################################################
#   # Dendogram
#   ############################################################################
#
#   tabPanel("Dendogram",
#            selectInput(inputId = "subject_info",
#                        label = "About Subject",
#                        choices = c("Subject Race", "Subject", "Type of Subject")),
#                        # selected = subject_race),
#            plotOutput(outputId = "dendo", height = "500px",
#                       width = "700px")
#   )
# )

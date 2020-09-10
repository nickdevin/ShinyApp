#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

fluidPage(
  titlePanel('Analysis of WTA Playing Styles, 2012-2020'),
  
  navlistPanel(
    widths = c(3, 9),
    tabPanel(
      "Introduction",
      textOutput('intro_text1'),
      br(),
      textOutput('intro_text2'),
      br(),
      textOutput('intro_text3')
    ),
    tabPanel(
      "Distributions",
      selectizeInput(
        inputId = 'ratio',
        label = 'Information to display',
        choices = c('Aggression', 'Consistency')
      ),
      selectizeInput(
        inputId = 'year2',
        label = 'Year',
        choices = 2012:2020
      ),
      plotOutput('ratios'),
      textOutput('ratios_text')
    ),
    tabPanel(
      "Analysis of Grand Slam Winners' Style",
      selectizeInput(
        inputId = 'player',
        label = 'Player',
        choices = GS_winners
      ),
      plotOutput("GS_winners"),
      textOutput('players_text')
    ),
    tabPanel(
      "Yearly Style Statistics",
      selectizeInput(
        inputId = 'year',
        label = 'Year',
        choices = unique(with_GS$year)
      ),
      fluidRow(column(6, plotOutput("GS")),
               column(6, plotOutput("pcts"))),
      br(),
      fluidRow(column(6, plotOutput("matchups")),
               column(6, plotOutput("counts")))
    ),
    tabPanel("Correlation Graph",
             fluidRow(column(
               9, plotOutput("correlation")
             )),
             textOutput('correlation_text'))
  )
)

# selectizeInput(
#   inputId = 'year',
#   label = 'Year',
#   choices = unique(with_GS$year)
# ),
# fluidRow(
#   selectizeInput(
#     inputId = 'ratio',
#     label = 'Information to display',
#     choices = unique(for_ratios_histogram$stat_type)
#   ),
#   plotOutput('ratios', width = '90%')
# ),
# fluidRow(column(6, plotOutput("correlation")),
#          column(6, plotOutput("counts"))),



# selectizeInput(
#   inputId = 'year',
#   label = 'Year',
#   choices = 2012:2020,
# )

#
# selectizeInput(inputId = 'year2',
#                label = 'Year',
#                choices = 2012:2020)
#
# selectizeInput(inputId = 'year3',
#                label = 'Year',
#                choices = 2012:2020)
#
# selectizeInput(inputId = 'year4',
#                label = 'Year',
#                choices = 2012:2020)
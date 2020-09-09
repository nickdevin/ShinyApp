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
    tabPanel("Component 1",
             selectizeInput(
               inputId = 'player',
               label = 'Player',
               choices = GS_winners
             ),
             plotOutput("GS_winners")),
    tabPanel("Component 2",
             selectizeInput(
               inputId = 'year',
               label = 'Year',
               choices = unique(with_GS$year)
             ),
             fluidRow(column(6, plotOutput("GS")),
                      column(6, plotOutput("pcts"))),
             plotOutput("matchups")),
    tabPanel("Component 3",
             selectizeInput(
               inputId = 'ratio',
               label = 'Information to display',
               choices = unique(for_ratios_histogram$stat_type)
             ),
             selectizeInput(inputId = 'year2',
                            label = 'Year',
                            choices = 2012:2020),
             plotOutput('ratios')),
    tabPanel("Component 4",
             fluidRow(column(6, plotOutput("correlation")),
                      column(6, plotOutput("counts")))),
    "-----",
    tabPanel("Component 5")
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
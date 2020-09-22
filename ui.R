#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

dashboardPage(
  skin = 'red',
  dashboardHeader(title = 'Analysis of WTA Player\nStyles, 2012-2020', titleWidth = 500),
  dashboardSidebar(
    width = 275,
    sidebarMenu(
      menuItem('Introduction',
               tabName = 'introduction',
               icon = icon('book')),
      menuItem(
        "Distributions",
        tabName = 'distributions',
        icon = icon('chart-bar')
      ),
      menuItem(
        "Analysis of Grand Slam Winners' Styles",
        tabName = 'gsstyles',
        icon = icon('trophy')
      ),
      menuItem(
        "Yearly Style Statistics",
        tabName = 'yearly',
        icon = icon('calendar-alt')
      ),
      menuItem(
        "Correlation Graph",
        tabName = 'correlation',
        icon = icon('chart-line')
      ),
      menuItem(
        "Conclusions",
        tabName = 'conclusions',
        icon = icon('lightbulb')
      )
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(
        tabName = 'introduction',
        h1('Introduction'),
        
        p(
          "In tennis, winning one of the four Grand Slam tournaments--the
        Australian Open, the French Open, Wimbledon, and the US Open--is
        considered the pinnacle of the sport among players, commentators,
        and fans alike. The 2010s have seen unprecedented parity among Grand
        Slam winners on the Women's Tennis Association (WTA); 19 different
        players won majors in the last decade, compared to 12 in both the 2000s
        and the 1990s, and only 7 in the 1980s."
        ),
        
        p(
          "Additionally, while Grand Slam
        winners have historically sustained long periods of dominance in the
        sport, recent winners such as Sloane Stephens, Jelena Ostapenko, and
        Garbine Muguruza have subsequently enjoyed only intermittent success."
        ),
        
        p(
          "The overall goal of this project is to categorize the playing styles
        of WTA players by two metrics, aggression and consistency, and to
        study the relationship between playing style and various aspects of
        \"success\", including percent of games won and likelihood of winning
        a Grand Slam tournament. I have defined aggression as the number of
        winners and unforced errors a player commits per game played, while
        consistency is the ratio of winners to unforced errors committed by a
        player. Players who commit more winners and errors per game than average
        are deemed aggressive, and otherwise are deemed defensive. Players with
        a ratio of winners to errors higher than the median are deemed
        consistent, and otherwise are deemed inconsistent. As a player's style
        can evolve over time, these classifications are made on a yearly basis."
        ),
        
        p(
          "The data used in this analysis is collected and generously made available
      for public use at:"
        ),
        HTML(
          "<a href = https://github.com/JeffSackmann>https://github.com/JeffSackmann</a>"
        )
      ),
      
      tabItem(
        tabName = 'distributions',
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
      
      tabItem(
        tabName = 'gsstyles',
        "Analysis of Grand Slam Winners' Style",
        selectizeInput(
          inputId = 'player',
          label = 'Player',
          choices = GS_winners
        ),
        plotOutput("GS_winners"),
        textOutput('players_text')
      ),
      
      tabItem(
        tabName = "yearly",
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
      
      tabItem(tabName = 'correlation',
              fluidRow(column(
                9, plotOutput("correlation")
              )),
              textOutput('correlation_text')),
      
      tabItem(
        tabName = 'conclusions',
        h1('Conclusions'),
        
        p(
          "It is conventional tennis wisdom that aggression is the key to
             success, and in the past, this appeared to be true. Looking at a
             list of all-time great players--Steffi Graf, Serena Williams,
             Martina Navratilova, Chris Evert--there seems to be ample
             evidence for this assertion. The data from recent years,
             however, does not support this, and instead lends credence
             to the old mantra: \"Consistency is key.\" In every year studied,
             consistent players, both aggressive and defensive, won more than
             50% of games played overall, as well as more than 50% of games
             played against inconsistent players."
        ),
        
        p(
          "In the years studied, both aggressive and defensive players won
             Grand Slams with regularity, while nearly all Grand Slams were won
             by consistent players. Additionally, players whose style varied
             frequently from year to year tended to win Grand Slams in years
             in which they were more consistent.The recent parity among Grand
             Slam winners could be explained by certain players' inability to
             maintain a high winner-to-error ratio from season to season."
        ),
        
        p(
          "The correlation graph revealed some interesting insights:
             the overall percentage of games won by aggressive players had a
             weak negative correlation with aggression, while the percentage of
             games won by defensive players had a weak-to-moderate positive
             correlation with aggression. This may indicate that being \"too
             aggressive\" or \"too defensive\" is not advantageous.
             Inconsistent players displayed a moderate-to-high correlation
             between win percentage and both number of winners hit per game and
             consistency, which may indicate that these are important skills for
             such players to focus on in order to improve results. Defensive
             consistent players' win percentage depends weakly on all four
             variables considered, and players may want to focus on this style
             of play to see dependable, steadily good results."
        ),
        
        h3("Where do we go from here?"),
        
        p(
          "There are many other factors that
             can contribute to a player's success, including serve statistics
             and the particular court surface on which a match is being played,
             and including these in the analysis would only enrich the results.
             It would be also be interesting to examine in greater depth the
             reasons behind the recent rise in defensive players. Common
             explanations (without evidence!) are player fitness and
             conditioning, slower court surfaces, and racket technology,
             all of which encourage players to engage in longer rallies from
             the baseline, rather than finish points quickly at net."
        )
      )
    )
  )
)




# fluidPage(
#   setBackgroundColor("eggshell"),
#   titlePanel('Analysis of WTA Playing Styles, 2012-2020'),
#   navlistPanel(
#     widths = c(3, 9),
#     tabPanel(
#       "Introduction",
#       "In tennis, winning one of the four Grand Slam tournaments--the
#         Australian Open, the French Open, Wimbledon, and the US Open--is
#         considered the pinnacle of the sport among players, commentators,
#         and fans alike. The 2010s have seen unprecedented parity among Grand
#         Slam winners on the Women's Tennis Association (WTA); 19 different
#         players won majors in the last decade, compared to 12 in both the 2000s
#         and the 1990s, and only 7 in the 1980s.",
#       br(),
#       br(),
#       "Additionally, while Grand Slam
#         winners have historically sustained long periods of dominance in the
#         sport, recent winners such as Sloane Stephens, Jelena Ostapenko, and
#         Garbine Muguruza have subsequently enjoyed only intermittent success.",
#       br(),
#       br(),
#       "The overall goal of this project is to categorize the playing styles
#         of WTA players by two metrics, aggression and consistency, and to
#         study the relationship between playing style and various aspects of
#         \"success\", including percent of games won and likelihood of winning
#         a Grand Slam tournament. I have defined aggression as the number of
#         winners and unforced errors a player commits per game played, while
#         consistency is the ratio of winners to unforced errors committed by a
#         player. Players who commit more winners and errors per game than average
#         are deemed aggressive, and otherwise are deemed defensive. Players with
#         a ratio of winners to errors higher than the median are deemed
#         consistent, and otherwise are deemed inconsistent. As a player's style
#         can evolve over time, these classifications are made on a yearly basis.",
#       br(),
#       br(),
#       "The data used in this analysis is collected and generously made available
#       for public use at:", 
#       HTML("<a href = https://github.com/JeffSackmann>https://github.com/JeffSackmann</a>")
#     ),
#     tabPanel(
#       "Distributions",
#       selectizeInput(
#         inputId = 'ratio',
#         label = 'Information to display',
#         choices = c('Aggression', 'Consistency')
#       ),
#       selectizeInput(
#         inputId = 'year2',
#         label = 'Year',
#         choices = 2012:2020
#       ),
#       plotOutput('ratios'),
#       textOutput('ratios_text')
#     ),
#     tabPanel(
#       "Analysis of Grand Slam Winners' Style",
#       selectizeInput(
#         inputId = 'player',
#         label = 'Player',
#         choices = GS_winners
#       ),
#       plotOutput("GS_winners"),
#       textOutput('players_text')
#     ),
#     tabPanel(
      # "Yearly Style Statistics",
      # selectizeInput(
      #   inputId = 'year',
      #   label = 'Year',
      #   choices = unique(with_GS$year)
      # ),
      # fluidRow(column(6, plotOutput("GS")),
      #          column(6, plotOutput("pcts"))),
      # br(),
      # fluidRow(column(6, plotOutput("matchups")),
      #          column(6, plotOutput("counts")))
#     ),
#     tabPanel(
      # "Correlation Graph",
      # fluidRow(column(9, plotOutput("correlation"))),
      # textOutput('correlation_text')
#     ),
#     tabPanel("Conclusions",
             # "It is conventional tennis wisdom that aggression is the key to
             # success, and in the past, this appeared to be true. Looking at a
             # list of all-time great players--Steffi Graf, Serena Williams,
             # Martina Navratilova, Chris Evert--there seems to be ample
             # evidence for this assertion. The data from recent years,
             # however, does not support this, and instead lends credence
             # to the old mantra: \"Consistency is key.\" In every year studied,
             # consistent players, both aggressive and defensive, won more than
             # 50% of games played overall, as well as more than 50% of games
             # played against inconsistent players.",
             # br(),
             # br(),
             # "In the years studied, both aggressive and defensive players won
             # Grand Slams with regularity, while nearly all Grand Slams were won
             # by consistent players. Additionally, whose style varied frequently.
             # from year to year tended to with Grand Slams in years that they
             # were consistent.The recent parity among Grand Slam winners may be
             # explained by certain players' inability to maintain a
             # high winner-to-error ratio from season to season.",
             # br(),
             # br(),
             # "The correlation graph revealed some interesting insights:
             # the overall percentage of games won by aggressive players had a
             # weak negative correlation with aggression, while the percentage of
             # games won by defensive players had a weak-to-moderate positive
             # correlation with aggression. This may indicate that being \"too
             # aggressive\" or \"too defensive\" is not advantageous.
             # Inconsistent players displayed a moderate-to-high correlation
             # between win percentage and both number of winners hit per game and
             # consistency, which may indicate that these are important skills for
             # such players to focus on in order to improve results. Defensive
             # consistent players' win percentage depends weakly on all four
             # variables considered, and players may want to focus on this style
             # of play to see dependable, steadily good results.",
             # br(),
             # br(),
             # "Where do we go from here? There are many other factors that
             # can contribute to a player's success, including serve statistics
             # and the particular court surface on which a match is being played,
             # and including these in the analysis would only enrich the results.
             # It would be also be interesting to examine in greater depth the
             # reasons behind the recent rise in defensive players. Common
             # explanations (without evidence!) are player fitness and
             # conditioning, slower court surfaces, and racket technology,
             # all of which encourage players to engage in longer rallies from
             # the baseline, rather than finish points quickly at net."
#              )
#   )
# )


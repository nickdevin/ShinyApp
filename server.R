#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

function(input, output) {
    desired_info = reactive(
        case_when(
            input$ratio == 'Aggression' ~ mean_per_game,
            input$ratio == 'Consistency' ~ winners_to_unforced_ratio
        )
    )
    
    output$GS = renderPlot(
        GS_plot(input$year, with_GS)
    )
    
    output$pcts = renderPlot(
        win_percent_plot(input$year, style_win_pcts)
    )
    
    output$matchups = renderPlot(
        matchups_plot(input$year, style_matchups)
    )
    
    output$correlation = renderPlot(
        corr_plot
    )
    
    output$counts = renderPlot(
        counts_plot
    )
    
    output$mean_per_game = renderPlot(
        aggro_plot(input$year, master_means)
    )
    
    output$ratios = renderPlot(
        means_plot(input$year2, for_ratios_histogram, info_list[[input$ratio]])
    )
    
    output$GS_winners = renderPlot(
        GS_winners_plot(input$player)
    )
    
    output$ratios_text = renderText(
        "The histograms above display the distributions of players'
        aggression and consistency in each year from 2012 to 2020. Recall that
        aggression is the ratio of a player's winners and errors to total
        games played, while consistency is the ratio of a player's winners to
        errors committed."
    )
    
    output$players_text = renderText(
        "The bar graphs above track the styles and overall percent of games
        won for a selection recent Grand Slam winners in each year from 2012 to
        2020. Bar height corresponds to the player's win percent, while the
        color of the bar corresponds to the player's style in the given year.
        Bars over years in which the player has won a grand slam are bordered in
        green."
    )
    
    output$correlation_text = renderText(
        "The chart above displays, within each of the four playing styles, 
        the linear correlation coefficient between a player's percentage of
        games won and each of four quantitative variables: aggression,
        consistency, mean number of winners hit per game, and mean number of
        errors committed per game. These figures are not divided by year."
    )
}

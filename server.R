#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

function(input, output) {
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
}

server <- function(input, output) {
  varInput <- reactive({
    switch(input$variable,
           "Labs" = Burroughs$Complete_Labs,
           "Within90" = Burroughs$Within90)
  })
  output$cont_table <- renderPrint({
    variable <- varInput()
    xtabs(~ variable + Year, Burroughs)
  })
  
  tabInput <- reactive({
    switch(input$variable,
           "Labs" = xtabs(~ Complete_Labs + Year, Burroughs),
           "Within90" = xtabs(~ Within90 + Year, Burroughs))
  })
  output$fisher_test <- renderPrint({
    table2x2 <- tabInput()
    exact2x2(table2x2, midp = input$midp)
  })
  
  aggInput <- reactive({
    switch(input$variable,
           "Labs" = Labs,
           "Within90" = within90_table)
  })
  output$plot_count <- renderPlot({
    aggData <- aggInput()
    ggplot(aggData) + geom_bar(aes(y = n, x = Year, fill = aggData[[2]]), stat = "identity", position = "dodge") + 
      labs(title = "Counts of 'Yes' vs 'No' Among New Patients\n For Selected Variable, By Year", y = "Count", fill = names(aggData[[2]])) +
      theme(plot.title = element_text(size = 14, hjust = 0.5), text = element_text(size = 12)) +
      scale_fill_manual(values = c("#000000", "#ccb85f"))
    })
  output$plot_prop <- renderPlot({
    aggData <- aggInput()
    ggplot(aggData) + geom_bar(aes(y = n, x = Year, fill = aggData[[2]]), stat = "identity", position = "fill") + 
      labs(title = "Proportion of 'Yes vs 'No' Among New Patients\n For Selected Variable, By Year", y = "Percent",
           fill = names(aggData[[2]])) +
      theme(plot.title = element_text(size = 14, hjust = 0.5), text = element_text(size = 12)) +
      scale_fill_manual(values = c("#000000", "#ccb85f"))
    })
  output$plot_donut <- renderPlot({
    aggData <- aggInput()
    ggplot(aggData) + geom_bar(aes(y = n, x = 1, fill = aggData[[2]]), stat = "identity", position = "fill") + coord_polar(theta = "y") + 
      facet_wrap(~ Year) + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), 
                                 panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                 panel.border = element_blank(), strip.background = element_blank(), 
                                 strip.text = element_blank()) + xlim(0, 1.5) + 
      geom_text(aes(x = 0, y = 0, label = Year), size = 5, fontface = "bold") + 
      labs(title = "'Yes' vs 'No' For the Selected Variable: \n Proportion of New Patients, By Year", 
           subtitle = "Note: Patient Data Collected October-December Within Each Year", 
           fill = names(aggData[[2]])) +
      theme(plot.title = element_text(size = 14, hjust = 0.5), 
            plot.subtitle = element_text(size = 12, hjust = 0.5),
            legend.position = "bottom", 
            text = element_text(size = 12)) +
      scale_fill_manual(values = c("#000000", "#ccb85f"))
  })
}

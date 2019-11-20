######################### Define SERVER logic ----##################
server <- function(input, output) {
  
  ############################# DOwnload data ######################
  # Reactive value for selected dataset ----
  datasetInput <- reactive({
    switch(input$downloadDataset,
           "olympics" = my_data)
  })
  
  # Table of selected dataset ----
  output$dataTable <- renderTable({
    datasetInput()
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$downloadDataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
  
  
  ############################## Regression Model####################
  # plot model
  #output$AIC <-plottStats(step3)#createERUI(plottStats(input$models=='Model1'))
  output$AIC <- renderPlot({
    input$go
    {if(input$models=='Model1'){plottStats(step1)
    } else if(input$models=='Model2'){plottStats(step2)
    } else if(input$models=='Model3'){plottStats(step3)
    }
    }
    #plottStats(step3)
  })
  # https://shiny.rstudio.com/reference/shiny/latest/plotOutput.html
  #input$newplot
  
  # texts output
  output$pred1p <- renderText({if(input$models=='Model1'){anova(step1)$'Pr(>F)'[1]}})  # call col name
  output$pred2p <- renderText({if(input$models=='Model2'){anova(step2)$'Pr(>F)'[1]}})
  output$pred3p <- renderText({if(input$models=='Model3'){anova(step3)$'Pr(>F)'[1]}})
  
  output$pred1slope <- renderText({if(input$models=='Model1'){step1[[1]][2]}})
  output$pred2slope <- renderText({if(input$models=='Model2'){step2[[1]][2]}})
  output$pred3slope <- renderText({if(input$models=='Model3'){step3[[1]][2]}})
  
  output$pred1intercept <- renderText({if(input$models=='Model1'){step1[[1]][1]}})
  output$pred2intercept <- renderText({if(input$models=='Model2'){step2[[1]][1]}})
  output$pred3intercept <- renderText({if(input$models=='Model3'){step3[[1]][1]}})
  
  output$pred1RSq <- renderText({if(input$models=='Model1'){summary(step1)[[8]][1]}})
  output$pred2RSq <- renderText({if(input$models=='Model2'){summary(step2)[[8]][1]}})
  output$pred3RSq <- renderText({if(input$models=='Model3'){summary(step3)[[8]][1]}})
  
  
  
  
  # previous
  pushButton <- eventReactive(input$go,{
    runif(input$num)
  })# take dependency on the input$go 
  
  output$conclude1 <- renderPrint({ 
    if(input$models=='Model1' & as.numeric(anova(step1)$'Pr(>F)'[1])<1-input$num){
      'Statistically Significant. Reject H0'} 
    else if(input$models=='Model2' & as.numeric(anova(step2)$'Pr(>F)'[1])<1-input$num){
      'Statistically Significant. Reject H0'} 
    else if(input$models=='Model3' & as.numeric(anova(step3)$'Pr(>F)'[1])<1-input$num){
      'Statistically Significant. Reject H0'} 
    else {
      'Statistically not significant. Do not reject H0'
    }
  })
  
  #################################### plot residuals###################################
  output$resid <- renderPlot({
    {if(input$models=='Model1'){par(mfrow = c(2, 2))
      plot(step1)
    } else if(input$models=='Model2'){par(mfrow = c(2, 2))
      plot(step2)
    } else if(input$models=='Model3'){par(mfrow = c(2, 2))
      plot(step3)
    }
    }
  })
  
  ######################## type math formual for Durbin Watson test########################
  output$HypoDurbin <- renderUI({
    withMathJax(helpText('$$H_0 : \\sigma_d = 0$$'),helpText('$$H_0 : \\sigma_d \\ne 0$$'))
  })
  # plot Durbin watson test
  output$DurbinStat <- renderText({if(input$models=='Model1'){durbinWatsonTest(step1)[[2]][1]
  }else if(input$models=='Model2'){durbinWatsonTest(step2)[[2]][1]
  }else if(input$models=='Model3'){durbinWatsonTest(step3)[[2]][1]
  }
  })
  
  output$DurbinProb <-   renderText({if(input$models=='Model1'){durbinWatsonTest(step1)[[3]][1]
  }else if(input$models=='Model2'){durbinWatsonTest(step2)[[3]][1]
  }else if(input$models=='Model3'){durbinWatsonTest(step3)[[3]][1]
  }
  })
  output$DurbinConclude <- renderPrint({ 
    if(input$models=='Model1' & as.numeric(durbinWatsonTest(step1)[[3]][1])<1-input$num){
      'Statistically Significant. Reject H0'} 
    else if(input$models=='Model2' & as.numeric(durbinWatsonTest(step2)[[3]][1])<1-input$num){
      'Statistically Significant. Reject H0'} 
    else if(input$models=='Model3' & as.numeric(durbinWatsonTest(step3)[[3]][1])<1-input$num){
      'Statistically Significant. Reject H0'} 
    else {
      'Statistically not significant. Do not reject H0'
    }
  })
  
  #################################### Create boxplot summary##############################
  list_continent = factor(my_data$continent)
  #levels(list_continent) # list of continents
  
  
  # Summary by medal
  cols <- c("continent") #,"medal"
  #cols = c("continent",input$checkbox)
  #print(cols)
  #sum_table <- my_data["continent",list(input$checkbox)]  %>% split(.$continent) %>% map(summary) 
  output$summary1 <- renderPrint({
    summary_cont = my_data[c(cols, input$checkbox),drop=FALSE]%>% split(.$continent) %>% map(summary)
    if (input$continent1 == "Africa"){
      summary_cont$Africa
    } else if (input$continent1 =="Americas"){
      summary_cont$Americas
    } else if (input$continent1 == "Asia"){
      summary_cont$Asia
    } else if (input$continent1 == "Europe"){
      summary_cont$Europe
    } else if (input$continent1 == "Oceania"){
      summary_cont$Oceania
    }
  })
  # https://shiny.rstudio.com/reference/shiny/latest/checkboxGroupInput.html
  # https://stackoverflow.com/questions/50225806/summary-for-multiple-specific-columns
 
  #################################### Create boxplot##########################
  # if graph need eventReactive
  dataplot <- eventReactive(input$checkbox2, {
    my_data <- my_data %>% filter(as.factor(continent) %in% c(input$checkbox2))
    #print(my_data)
  })
  
  plot2 = reactive({
    if (input$columnsGraph == 'medal'){
        print(dataplot())
         ggplot(data=dataplot(), aes(x=dataplot()$continent, y=medal,fill=dataplot()$continent)) +
         geom_boxplot()+
         #scale_fill_viridis(discrete=TRUE,alpha=1, option="A")+
         geom_jitter(color="black",size = 0.4,alpha =0.9)+
         theme_ipsum() +
         xlab("Continents") + ylab("medal") +
         theme(
           legend.position="none",
           plot.title = element_text(size=11)
          ) +
          ggtitle("Continents chosen vs y")
    } else if (input$columnsGraph == 'Population_total'){
      print(dataplot())
      ggplot(data=dataplot(), aes(x=dataplot()$continent, y=Population_total,fill=dataplot()$continent)) +
        geom_boxplot()+
        #scale_fill_viridis(discrete=TRUE,alpha=1, option="A")+
        geom_jitter(color="black",size = 0.4,alpha =0.9)+
        theme_ipsum() +
        xlab("Continents") + ylab("Population_total") +
        theme(
          legend.position="none",
          plot.title = element_text(size=11)
        ) +
        ggtitle("Continents chosen vs y")
    } else if (input$columnsGraph == 'Life_exp'){
      print(dataplot())
      ggplot(data=dataplot(), aes(x=dataplot()$continent, y=Life_exp,fill=dataplot()$continent)) +
        geom_boxplot()+
        #scale_fill_viridis(discrete=TRUE,alpha=1, option="A")+
        geom_jitter(color="black",size = 0.4,alpha =0.9)+
        theme_ipsum() +
        xlab("Continents") + ylab("Life_exp") +
        theme(
          legend.position="none",
          plot.title = element_text(size=11)
        ) +
        ggtitle("Continents chosen vs y")
    } else if (input$columnsGraph == 'Health_exp'){
      print(dataplot())
      ggplot(data=dataplot(), aes(x=dataplot()$continent, y=Health_exp,fill=dataplot()$continent)) +
        geom_boxplot()+
        #scale_fill_viridis(discrete=TRUE,alpha=1, option="A")+
        geom_jitter(color="black",size = 0.4,alpha =0.9)+
        theme_ipsum() +
        xlab("Continents") + ylab("Health_exp") +
        theme(
          legend.position="none",
          plot.title = element_text(size=11)
        ) +
        ggtitle("Continents chosen vs y")
    } else if (input$columnsGraph == 'GDP_per_cap'){
      print(dataplot())
      ggplot(data=dataplot(), aes(x=dataplot()$continent, y=GDP_per_cap,fill=dataplot()$continent)) +
        geom_boxplot()+
        #scale_fill_viridis(discrete=TRUE,alpha=1, option="A")+
        geom_jitter(color="black",size = 0.4,alpha =0.9)+
        theme_ipsum() +
        xlab("Continents") + ylab("GDP_per_cap") +
        theme(
          legend.position="none",
          plot.title = element_text(size=11)
        ) +
        ggtitle("Continents chosen vs y")
    } else if (input$columnsGraph == 'cpi'){
      print(dataplot())
      ggplot(data=dataplot(), aes(x=dataplot()$continent, y=cpi,fill=dataplot()$continent)) +
        geom_boxplot()+
        #scale_fill_viridis(discrete=TRUE,alpha=1, option="A")+
        geom_jitter(color="black",size = 0.4,alpha =0.9)+
        theme_ipsum() +
        xlab("Continents") + ylab("cpi") +
        theme(
          legend.position="none",
          plot.title = element_text(size=11)
        ) +
        ggtitle("Continents chosen vs y")
    } else if (input$columnsGraph == 'hdi'){
      print(dataplot())
      ggplot(data=dataplot(), aes(x=dataplot()$continent, y=hdi,fill=dataplot()$continent)) +
        geom_boxplot()+
        #scale_fill_viridis(discrete=TRUE,alpha=1, option="A")+
        geom_jitter(color="black",size = 0.4,alpha =0.9)+
        theme_ipsum() +
        xlab("Continents") + ylab("hdi") +
        theme(
          legend.position="none",
          plot.title = element_text(size=11)
        ) +
        ggtitle("Continents chosen vs y")
    } else if (input$columnsGraph == 'homeadvantage'){
      print(dataplot())
      ggplot(data=dataplot(), aes(x=dataplot()$continent, y=homeadvantage,fill=dataplot()$continent)) +
        geom_boxplot()+
        #scale_fill_viridis(discrete=TRUE,alpha=1, option="A")+
        geom_jitter(color="black",size = 0.4,alpha =0.9)+
        theme_ipsum() +
        xlab("Continents") + ylab("homeadvantage") +
        theme(
          legend.position="none",
          plot.title = element_text(size=11)
        ) +
        ggtitle("Continents chosen vs y")
    }
    
  })
  
  ###################### bar graph version #####################
  plot3 = reactive({
    if (input$columnsGraph == 'medal'){
      ggplot(data=dataplot(), aes(x=dataplot()$continent, y=medal, fill=dataplot()$continent)) +
      geom_bar(stat="identity") +
      theme_ipsum()+
      xlab("Continents") + ylab("medal") 
    } else if (input$columnsGraph == 'Population_total'){
      ggplot(data=dataplot(), aes(x=dataplot()$continent, y=Population_total, fill=dataplot()$continent)) +
        geom_bar(stat="identity") +
        theme_ipsum()+
        xlab("Continents") + ylab("Population_total") 
    } 
    else if (input$columnsGraph == 'Life_exp'){
      ggplot(data=dataplot(), aes(x=dataplot()$continent, y=Life_exp, fill=dataplot()$continent)) +
        geom_bar(stat="identity") +
        theme_ipsum()+
        xlab("Continents") + ylab("Life_exp") 
    } else if (input$columnsGraph == 'Health_exp'){
      ggplot(data=dataplot(), aes(x=dataplot()$continent, y=Health_exp, fill=dataplot()$continent)) +
        geom_bar(stat="identity") +
        theme_ipsum()+
        xlab("Continents") + ylab("Health_exp") 
    } else if (input$columnsGraph == 'GDP_per_cap'){
      ggplot(data=dataplot(), aes(x=dataplot()$continent, y=GDP_per_cap, fill=dataplot()$continent)) +
        geom_bar(stat="identity") +
        theme_ipsum()+
        xlab("Continents") + ylab("GDP_per_cap") 
    } else if (input$columnsGraph == 'cpi'){
      ggplot(data=dataplot(), aes(x=dataplot()$continent, y=cpi, fill=dataplot()$continent)) +
        geom_bar(stat="identity") +
        theme_ipsum()+
        xlab("Continents") + ylab("cpi") 
    } else if (input$columnsGraph == 'hdi'){
      ggplot(data=dataplot(), aes(x=dataplot()$continent, y=hdi, fill=dataplot()$continent)) +
        geom_bar(stat="identity") +
        theme_ipsum()+
        xlab("Continents") + ylab("hdi") 
    } else if (input$columnsGraph == 'homeadvantage'){
      ggplot(data=dataplot(), aes(x=dataplot()$continent, y=homeadvantage, fill=dataplot()$continent)) +
        geom_bar(stat="identity") +
        theme_ipsum()+
        xlab("Continents") + ylab("homeadvantage") 
    } 
  })
  ################## Choose between Boxplot or barplot#################
  # Return the requested graph
  graphInput <- reactive({
    switch(input$graphtype,
           "Boxplot" = plot2(),
           "Bar" = plot3()
    )
  })
  output$selected_graph <- renderPlot({ 
    graphInput()
  })
  #https://stackoverflow.com/questions/48312392/shiny-allow-users-to-choose-which-plot-outputs-to-display
  # https://www.r-graph-gallery.com/boxplot.html

  ########################## World Map #################################
  output$FuckShiny = renderLeaflet({
    yy <- input$Nice
    if(yy=="2000"){
      medal <- medal2000
    } else if(yy=="2004"){
      medal <- medal2004
    } else if(yy=="2008"){
      medal <- medal2008
    } else if(yy=="2012"){
      medal <- medal2012
    } else{
      medal <- medal2016
    }
    tm <- tm_shape(medal) + tm_polygons("medal", palette = "OrRd") + tm_layout(title = "Medal Tally Prediction based on Spatial view", frame = FALSE) + tm_scale_bar(breaks = c(0, 100, 200), text.size =2)
    
    tmap_leaflet(tm)
  })
  
  ########################## Scatterplot ###################
  # Get the data from the variables declared on the ui.R file
  df <- reactive({olympics[, c(input$xCol, input$yCol)]})
  
  # Create the plot
  output$plot <- renderPlotly({ggplot(data = olympics, aes_string(x = input$xCol, y = input$yCol, Country = olympics$Country_Name, Year = olympics$Year)) +
      geom_point(color="orange")+theme(axis.text.x = element_text(size  = 6,angle = 45,hjust = 1, vjust = 1))+theme(axis.text.y = element_text(size=6))})
  
  
  ######################## predictive chart #####################
  plotType <- function(x,type) {
    switch(type,
           Top3 = (ggplot(top3,aes(top3$Country_name,top3$X2020)) + geom_col(fill="steelblue") +
                     labs( y = "Weighted Medal Tally",x="Country Name")+ ggpubr::rotate_x_text() ),
           Top5 = (ggplot(top5,aes(top5$Country_name,top5$X2020)) + geom_col(fill="steelblue") +
                     labs(y = "Weighted Medal Tally",x="Country Name") + ggpubr::rotate_x_text()),
           Top10 = (ggplot(top10,aes(top10$Country_name,top10$X2020)) + geom_col(fill="steelblue") +
                      labs(y = "Weighted Medal Tally",x="Country Name")+ ggpubr::rotate_x_text() ) )
  }
  # Plot for the historical medal tally 
  output$medalPlot <- renderPlot({
    
    par(mar = c(10, 4, 4, 0))
    ggplot(medaldata,aes(medaldata$Country_name,medaldata[,input$year])) +geom_col(fill="steelblue") + ggpubr::rotate_x_text() +
      labs( y = "Weighted Medal Tally",x="Country Name") + geom_text(aes(label=medaldata[,input$year]), vjust=-0.3, color="black", size=2)
    
  })
  # Plot for 2020 Prediction only
  output$rankedPlot <- renderPlot({
    plotType(medaldata,input$pType)
    
  })
} # end of server

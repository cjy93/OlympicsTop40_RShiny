# UI Shiny
################### Define UI ----############################
css <- HTML(" body {
    background-color: #000000;
}")
ui <- dashboardPage(skin = 'green',
                    dashboardHeader(title = 'Predicting Olympics 2020', titleWidth = 400),
                    dashboardSidebar(width = 300,
                                     sidebarMenu(id = 'sbm',
                                                 menuItem('Home', tabName = 'Home', icon=icon("home")),
                                                 menuItem('Download', tabName = 'Download', icon = icon("download")),
                                                 menuItem('Descriptive_WorldMap', tabName = 'Descriptive_WorldMap', icon = icon("globe")),
                                                 menuItem('Descriptive_2_var', tabName = 'Descriptive_2_var', icon = icon("chart-bar")),
                                                 menuItem('Descriptive_Multiple', tabName = 'Descriptive', icon = icon("chart-bar")),
                                                 menuItem('Inferential', tabName = 'Inferential', icon = icon("chart-line")),
                                                 menuItem('Predictive', tabName = 'Predictive', icon = icon("chart-line")) # tabs are here!
                                     )
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = 'Home',
                                fluidPage(
                                  titlePanel('Home'),
                                  sidebarLayout(
                                    sidebarPanel(
                                      h2("Installation"),
                                      p("Shiny is available on CRAN, so you can install it in the usual way from your R console:"),
                                      code('install.packages("shiny")',
                                           'install.packages(dplyr)',
                                           'install.packages(reshape2)',
                                           'install.packages(ggplot2)',
                                           'install.packages(shiny)',
                                           'install.packages(shinydashboard)',
                                           'install.packages(MASS)',
                                           'install.packages(png)',
                                           'install.packages(jpeg)',
                                           'install.packages(ERSA)',
                                           'install.packages(car)',
                                           'install.packages(tidyverse)',
                                           'install.packages(hrbrthemes)',
                                           'install.packages(viridis)',
                                           'install.packages(countrycode)',
                                           'install.packages(purrr)',
                                           'install.packages(datasets)',
                                           'install.packages(tmap)',
                                           'install.packages(leaflet)',
                                           'install.packages(readr)',
                                           'install.packages(plotly)'),
                                      br(),
                                      br(),
                                      br(),
                                      br(),
                                      img(src = "Inferential-Statistics.png", height = 70, width = 200),
                                      br(),
                                      "Shiny is a product of ", 
                                      span("RStudio", style = "color:blue")
                                    ),
                                    mainPanel(
                                      img(src = "2020_1.png", height = 140, width = 400),
                                      h1("Citius, Altius, Fortius: Success at the Olympics"),
                                      p("Every country contends with equal zest to triumph at the Olympics, 
                                      yet some countries outshine the others. This probed us to investigate the 
                                      factors that distinguish the wins from the losses. We examined the role of 
                                      various socio-economic and political factors that influence the medal tally of 
                                      a nation. These factors include national income, population, human development, 
                                      health expenditure per capita, life expectancy, corruption, connectivity, 
                                      political regime, and host country advantage. This panel regression is conducted 
                                      using data from the year 1996 to 2016."),
                                      h2("Data Overview"),
                                      p("Our significant variables are GDP, population, HDI, population*life expectancy, home advantage*GDP per capita, home advantage*GDP PPP. 
                                        In addition, we have also provided out of sample predictions for the 2020 Tokyo Olympics."),
                                      br(),
                                      strong("Important Notes"),
                                      p("The dependent variable Medal Tally is a weighted average of the gold, silver and bronze medals, 
                                        with the following weights associated with the three categories respectively; 
                                        half, one third, and one sixth."),
                                      p("The independent variables are lagged by four years under the 
                                        assumption that four years are required to build, develop and train 
                                        the athletes."),
                                      img(src = "Data-sources.jpg"),
                                      #imageOutput("Data-sources.jpg"),
                                      p("For an introduction olympic games, visit the official website ",
                                        a("Olympics Homepage.", 
                                          href = "https://www.olympic.org/")),
                                      br()
                                      
                                    )
                                  )
                                )
                           ),#end of tabname "home
                        tabItem(tabName = 'Descriptive_WorldMap',
                                fluidPage(
                                  titlePanel("Performances of Top 40 Countries In Olympics"),
                                  sidebarLayout(
                                    sidebarPanel(
                                      selectInput(inputId = "Nice",
                                                  label = "Choose the Year of Olympic you want to see:",
                                                  choices = list("2000" ,"2004", "2008", "2012", "2016")
                                      )
                                    ),
                                    mainPanel(
                                    leafletOutput("FuckShiny")
                                    )
                                  )
                                )
                        ),#end of tabname "Descriptive_WorldMap"
                        
                        tabItem(tabName = 'Descriptive_2_var',
                                fluidPage(
                                  titlePanel("Interactive ScatterPlot"),
                                  sidebarLayout(
                                    sidebarPanel(
                                      selectInput('xCol', 'X', names(olympics)),
                                      selectInput('yCol', 'Y', names(olympics))
                                      ),
                                    mainPanel(
                                      plotlyOutput('plot')
                                    )
                                  )
                                )
                              ),#end of tabname "Descriptive_2_var"
                        
                        
                        
                        tabItem(tabName = 'Download',
                                fluidPage(
                                  titlePanel('Downloading Dataset'),
                                  sidebarLayout(
                                    sidebarPanel(
                                      h2("Download Dataset"),
                                      # Input: Choose dataset ----
                                      selectInput("downloadDataset", "Choose a dataset:",
                                                  choices = c("olympics")),
                                      # Button
                                      downloadButton("downloadData", "Download")
                                      ),
                                     
                                    mainPanel(
                                      tableOutput("dataTable")
                                    )
                                  )
                                )
                        ),#end of tabname "download"
                        
                        tabItem(tabName = 'Descriptive',
                                fluidPage(
                                  titlePanel('Boxplot Descriptive statistical table and plot'),
                                  sidebarLayout(
                                    sidebarPanel(
                                      selectInput(inputId = "continent1", 
                                                  label = "Choose a continent(for summary table)",
                                                  choices = c("Africa", 
                                                              "Americas",
                                                              "Asia",
                                                              "Europe",
                                                              "Oceania"),
                                                  selected = "Americas"),
                                      # Checkboxes for cols to use
                                      checkboxGroupInput(inputId ="checkbox", label = "Checkbox group(for summary table", 
                                                         choices = c("Population_total", "Life_exp" , "Health_exp",
                                                                        "GDP_per_cap","cpi", "hdi", "homeadvantage","medal"),
                                                         selected = "medal"),
                                      hr(),
                                      # Checkboxes for number of boxplot on the boxplot
                                      checkboxGroupInput(inputId ="checkbox2", label = "Checkbox continent(for graph)", 
                                                         choices = c("Africa", 
                                                                     "Americas",
                                                                     "Asia",
                                                                     "Europe",
                                                                     "Oceania"),
                                                         selected = c("Americas","Africa","Asia")),
                                      selectInput(inputId = "columnsGraph", 
                                                  label = "Choose a dependent variable(for graph)",
                                                  choices = c("Population_total", "Life_exp" , "Health_exp",
                                                              "GDP_per_cap","cpi", "hdi", "homeadvantage","medal"),
                                                  selected = "medal"),
                                      # RadioButton Choose graph type
                                      radioButtons("graphtype", label = "Choose graph type",
                                                   choices = list("Boxplot","Bar"), 
                                                   selected = "Boxplot"),
                                      
                                      hr()
                                    ),
                                    mainPanel(
                                      h4("Descriptive Statistics"),
                                      verbatimTextOutput("summary1"),
                                      verbatimTextOutput("checkboxes"),
                                      #plotOutput("plot2"),
                                      plotOutput("selected_graph")
                                    )
                                  )
                                )
                                
                        ),#end of tabname "descriptive"
                        
                        tabItem(tabName = 'Inferential',
                                fluidPage(
                                  titlePanel('Polynomial Regression Model'),
                                  sidebarLayout(
                                    sidebarPanel(
                                      selectInput(inputId = "models", 
                                                  label = "Choose a variable to display",
                                                  choices = c("Model1", 
                                                              "Model2",
                                                              "Model3"),
                                                  selected = "Model3"),
                                      
                                      numericInput(inputId = "num", 
                                                   label = ("confidence level"), 
                                                   value = 0.95),
                                      actionButton(inputId = "go",
                                                   label = "Update"),
                                      hr()
                                      
                                      
                                    ),
                                    
                                    # fluidRow(
                                    #   column(width = 12,
                                    #          box(
                                    #            width = 12,
                                    #            height = 800,
                                    #            solidHeader = TRUE,
                                    #            collapsible = FALSE,
                                    #            collapsed = FALSE,
                                    #            plotOutput('AIC', height = 750)
                                    #          )
                                    #   )
                                    # ), #end of fluidRow
                                    
                                    # tab at the bottom
                                    mainPanel(
                                      #h5("The chart shows the relationship between the chosen demographic variable and total sales value"),
                                      h5("The polynomial regression model (bivariate analysis) is calculated for each demographic variable and shown below:"),                                               
                                      h4("Slope"), 
                                      verbatimTextOutput("pred1slope"),
                                      verbatimTextOutput("pred2slope"),
                                      verbatimTextOutput("pred3slope"),
                                      h4("Intercept"),
                                      verbatimTextOutput("pred1intercept"),
                                      verbatimTextOutput("pred2intercept"),
                                      verbatimTextOutput("pred3intercept"),
                                      h4("R Square"),
                                      verbatimTextOutput("pred1RSq"),
                                      verbatimTextOutput("pred2RSq"),
                                      verbatimTextOutput("pred3RSq"),
                                      h5("If p value is < 1-(confidence level), we conclude that the relationship between the independent variables and medal average is statistically significant"),
                                      h4("P value of regression model"),
                                      verbatimTextOutput("pred1p"),
                                      verbatimTextOutput("pred2p"),
                                      verbatimTextOutput("pred3p"),
                                      verbatimTextOutput("conclude1"),
                                      verbatimTextOutput("conclude2"),
                                      verbatimTextOutput("conclude3"),
                                      #plottStats("finalCoef")
                                      plotOutput("AIC"),
                                      h4("In order for our regression model to be more accurate, our group has to check if the assumption of error terms having constant variance is satisfied."),
                                      plotOutput("resid"),
                                      column(6,
                                             h4("Now we check if the error terms are independent at 5% significance level using the Durbin Watson test."),
                                             h4("The Durbin_Watson Test"),
                                             h5("The value of Durbin-Watson Statistics ranges from 0 to 4.  
                                         As a general Rule of thumb, the residuals are not correlated if the DW statistic is approximately 2 ,
                                         and an acceptable range for the DW statistic is 1.50 to 2.50"),
                                             uiOutput("HypoDurbin")
                                      ),
                                      column(6,
                                             h4('Durbin-Watson Statistic'),
                                             verbatimTextOutput("DurbinStat"),
                                             h4('Durbin-Watson P-value'),
                                             verbatimTextOutput("DurbinProb"),
                                             h4('Durbin-Watson conclusion'),
                                             verbatimTextOutput("DurbinConclude")
                                      )
                                    )
                                  )) ),#end of tabname "Inferential"
                                
                        
                        
                        tabItem(tabName = 'Predictive',
                                fluidPage(
                                  titlePanel("Predictive Model"),
                                  sidebarLayout(   
                                    sidebarPanel(
                                      selectInput("year", "1st Tab: Distribution of Countries by Year", 
                                                  choices=c("2000","2004","2008","2012","2016","2020")),
                                      radioButtons("pType","2nd Tab: 2020 Predicted Countries",
                                                   list("Top3","Top5","Top10")),
                                      hr(),
                                      helpText("Overall trend with respect to top 40 countries from 2000 to 2020 and a deep dive into Predicted winning countries in 2020 Olympics")
                                    ),
                                    mainPanel(
                                      tabsetPanel(
                                        tabPanel("Distribution of Countries by Year",plotOutput("medalPlot")),  
                                        tabPanel("2020 Predicted countries",plotOutput("rankedPlot"))
                                      )
                                    )
                                  )
                                )
                        )#end of tabname "Predictive"
              
            )
            
          ) 
        )# end of UI


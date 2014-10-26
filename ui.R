
shinyUI(navbarPage("My Shiny App",
                   
                   tabPanel("Start",
                            #contains the welcoming stuff & help
                            sidebarLayout(
                              mainPanel(
                                h1("Help"), br(),
                                "This little app gives you an insight in the possibilities of the shiny package for R. ", br(),
                                "The app has three pages. Two are working with the", code("mtcars")," data set.",br(),br(),
                                h3("Start Page"),br(),
                                "This is the landing page. Hit the welcome button for a little gimmick.",
                                br(),br(),
                                h3("Cars"),
                                "This page is split with a tabsetpanel. Click on cars to get to this part of the app.",br(), h4("Cylinders Tab"),
                                "Here you can create reactive plots to visualize differences between cars with different number of cylinders.",br(),
                                "You have the option to add boxplots and the mean for each group of cars.",br(),
                                "The plots are made with the ggplot2 package and the data is provided with the reactive function in the server file.",br(),br(),
                                h4("Clustering & Comparison Tab"),
                                "The second tabpanel gives you the option to perform a cluster analysis on the data and compare two models.",br(),
                                "You can choose how the app calculates the difference (eucledean, manhattan,...)",
                                "The dendogramm responds instantly.",br(),
                                "Also you can compare two cars with the select input fields.",br(),
                                "The comparsion becomes interesting if you choose two cars that are in a cluster with a certain method, but totally different in a different one.",br()),
                            
                            sidebarPanel(
                            actionButton("btn",label = "Welcome"),
                            h3(textOutput("welcome")),
                            textOutput("language")))),
                   tabPanel( 
                     #contains another tabsetpanel for the cylinders and clustering & comparison page
                    "Cars",
                    tabsetPanel(
                      tabPanel("Cylinders",
                               #first tabpanel
                        sidebarLayout(
                          sidebarPanel("Options",
                                       checkboxGroupInput("cars_cyl", label ="Cylinder", choices = levels(factor(mtcars$cyl))),
                                       checkboxGroupInput("cars_plot",label = "Plot extras", choices = list("Boxplot"=1, "Connect means"=2),selected =1 )
                                       ),
                          mainPanel(plotOutput("carplot"), dataTableOutput("cartable")))),
                    tabPanel("Clustering & Comparison",
                             #second tabpanel
                             sidebarLayout(
                               sidebarPanel(h4("Distance calculation"),
                                            selectInput("distselect", label ="Algorithm", choices = c("euclidean", "maximum","manhattan", "canberra", "binary", "minkowski")),
                                            h4("Comparison"),
                                            selectInput("car1select",label = "Car 1",choices = rownames(mtcars)),
                                            selectInput("car2select", label = "Car 2", choices = rownames(mtcars))
                                             
                               ),
                               mainPanel(plotOutput("dendogram"), br(), h3("Car 1"),tableOutput("car1stats"),
                                         br(), h3("Car 2"), tableOutput("car2stats")))     
                   ))
  )))
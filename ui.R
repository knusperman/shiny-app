
shinyUI(navbarPage("My Shiny App",
                   
                   tabPanel("Start",
                            #contains just the welcoming stuff
                            mainPanel(
                            actionButton("btn",label = "Welcome"),
                            h3(textOutput("welcome")),
                            textOutput("language"))),
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
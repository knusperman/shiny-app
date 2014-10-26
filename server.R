

#load at launch
library(ggplot2)
library(ggdendro)

shinyServer(function(input, output, session) {

  cardata <- reactive({
    #only runs when input cars_cyl changes
    mtcars[mtcars$cyl %in% input$cars_cyl,]
  })
  output$carplot <- renderPlot({
    #standard plot depending on reactive cardata()
    plot <- ggplot(cardata(), aes(x = cyl, y = mpg, color = factor(cyl)))+geom_point()+xlab("cylinder")
    if( 1 %in% input$cars_plot){
      #add boxplots
      plot <- plot + geom_boxplot()
    }
    if ( 2 %in% input$cars_plot){
      #add means
      plot <- plot + stat_summary(fun.y=mean, geom="line", aes(group =1))+stat_summary(fun.y=mean, geom="point", shape = 18, size = 6)
    }
      #plot it
    print(plot)
    })
  output$cartable <- renderDataTable({
    c <- cardata()
    # add rownames to data.frame
    c <- cbind(rownames(c), c)
    colnames(c)<- c("car", colnames(cardata()))
    c
  })
  #clustering
  output$dendogram <- renderPlot({
    #depends on distance algorithm selected in sidebar
    distance <- dist(as.matrix(mtcars), method = input$distselect)
    #perform clustering
    clusters <- hclust(distance)
    #plot the dendogram
    print(ggdendrogram(clusters))
  })
  
  #comparison
  output$car1stats <- renderTable({
    mtcars[rownames(mtcars)==input$car1select,]
  })
  
  output$car2stats <- renderTable({
    mtcars[rownames(mtcars)==input$car2select,]
  })
  
  # welcome logic
  output$welcome <- renderText({hello()[2]})
  output$language <- renderText(hello()[1])
  
  hello <- reactive(
    if(input$btn){
      switch(sample(1:19,size=1),
           "1"=  c("French" ,"Bonjour" ),
           "2"= c( "Spanish" , "Hola" ),
           "3"=  c("Italian" , "Bon Giorno" ),
           "4"=  c("German" , "Guten Tag "),
           "5"=  c("Chinese" , "Ni hao "),
           "6"=  c("Irish" ," Dia Duit "),
           "7"=  c( "Hindi" , "Namaste "),
           "8"=  c( "Russian" , "Zdravstvuite "),
           "9"=  c( "Greek" , "Yia sou (Ya-soo) "),
           "10"=  c("Czech" , "Dobry rano "),
           "11"=  c("Japanese" , "Ohayou gozaimasu"),
           "12"=  c( "Hebrew" , "Shalom" ),
           "13"=  c( "Arabic,based languages" , "Marhabah" ),
           "14"= c("Swedish" , "Hej "),
           "15"= c("Dutch" , "Goedendag") ,
           "16"=   c("Swahili" , "Jambo" ),
           "17"=   c("Vietnamese" , "Chao "),
           "18"=   c("Korean" , "Ahn nyeong ha se yo "),
           "19"=   c("Portuguese" , "Bom dia "))
    }
  )
})
  
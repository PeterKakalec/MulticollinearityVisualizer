#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(ggforce)

GenParams <- function(yX1Dist,yX2Dist,x1X2Dist){
  newDat <- data.frame(yX1Dist,yX2Dist,x1X2Dist)
  return(newDat)
}
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Multicollinearity Demo"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("yX1Dist",
                  "%Overlap Y and X1",
                  min = 0,
                  max = 100,
                  value = 50,
                  step=1),
      sliderInput("yX2Dist",
                  "%Overlap Y and X2",
                  min=0,
                  max=100,
                  value=50,
                  step=1),
      sliderInput("x1X2Dist",
                  "Multicollinearity",
                  min=0,
                  max=100,
                  value=50,
                  step=1)
    ),
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  params <- reactive ({
    GenParams(input$yX1Dist,input$yX2Dist,input$x1X2Dist)
  })
  makeCircles <- reactive({
    #converts degrees to radians for rotating X2's position
    d2r <- 0.0174533
    
    #defining the radius for our circles
    rUnit <- 1
    
    #gets %distance from inputs and converts to %distance (i.e 0% overlap = 100% distance)
    distDat<-params()
    
    #Constants to move our circles by the correct amount. Out of 200 because its it's a % distance of radius. (200% = distance of 2x radius = 0 overlap)
    dists <- dists<-c(0,1.5,3.1,4.7,6.2,7.8,9.4,11,12.6,14.1,15.7,17.3,18.8,20.4,22,23.6,25.2,26.8,28.3,29.9,31.5,
                      33.2,34.7,36.3,37.9,39.5,41.1,42.7,44.3,46,47.5,49.2,50.8,52.4,54.1,55.7,57.3,
                      59,60.6,62.3,63.9,65.6,67.3,68.9,70.6,72.3,74,75.7,77.3,79.1,80.8,82.5,84.2,
                      86,87.7,89.5,91.2,93,94.8,96.6,98.4,100.2,102,103.8,105.7,107.5,109.4,111.3,113.2,
                      115.1,117,119,121,122.9,124.9,126.9,129,131,133.1,135.3,137.4,139.6,141.8,144,146.4,
                      148.6,151,153.4,155.9,158.5,161.1,163.8,166.6,169.4,172.5,175.7,179.1,182.8,186.9,191.7,200)
    
    #Taking input and converting to actual % of radius
    x1ydist=dists[(101-distDat$yX1Dist)]
    x2ydist=dists[(101-distDat$yX2Dist)]
    x1x2dist=dists[(101-distDat$x1X2Dist)]
    
    #Placing Y at a static location
    yx <- 15
    yy <- 20
    y <- data.frame("x"=yx,"y"=yy)
    
    #calculating y angle relative to X2's position at 90-degrees off of X1
    angleDat <- data.frame("x1x2"=(x1x2dist/100)*rUnit,"x1y"=(x1ydist/100)*rUnit,"x2y"=(x2ydist/100)*rUnit)
    yAngle <- (90*d2r)-acos(((angleDat$x1y^2)+(angleDat$x1x2^2)-(angleDat$x2y^2))/(2*(angleDat$x1y*angleDat$x1x2)))
    
    #Placing X1 at the correct angle and distance from Y while maintaining its distance with X2
    x1x <- y$x-(((x1ydist/100)*rUnit)*sin(yAngle))
    x1y <- y$y-(((x1ydist/100)*rUnit)*cos(yAngle))
    
    #Fix to display x1 when its perfectly associated with y, and y's association with X2 is equal to X1 and X2's
    if(distDat$yX1Dist==100 && distDat$yX2Dist == distDat$x1X2Dist){
      x1x<-y$x
      x1y<-y$y
    }
    x1 <- data.frame("x" = x1x, "y" = x1y)
    
    #Placing X2 in a way which keeps it at a constant 90-degree angle from X1 (i.e. only horizontal movement)
    x2x <- x1$x + ((x1x2dist/100)*rUnit)
    x2y <- x1$y
    
    #Fix to display circles in situations of perfect multicollinearity, when yx1 association = yx2 association
    if(distDat$yX1Dist==distDat$yX2Dist && distDat$x1X2Dist==100){
      x1x<-y$x
      x1y<-y$y-(x1ydist/100)*rUnit
      x1 <- data.frame("x" = x1x, "y" = x1y)
      x2x<-y$x
      x2y<-y$y-(x2ydist/100)*rUnit
    }
    x2 <- data.frame("x"=x2x,"y"=x2y)

    #Putting circle positions in a data frame and returning it
    df.venn <- data.frame(x = c(x1$x, x2$x, y$x),
                          y = c(x1$y, x2$y, y$y),
                          labels = c('x1', 'x2', 'y'))
    return(df.venn)
  })
  
  output$distPlot <- renderPlot({
    diagramData<-makeCircles()
    ggplot(diagramData, aes(x0 = x, y0 = y, r = 1, fill = labels)) +
      geom_circle(alpha = .3, size = 1, colour = 'grey') +
      coord_fixed() +
      scale_x_continuous(limits=c(12,18))+
      scale_y_continuous(limits=c(17,21.2))+
      theme_void()+
      geom_text(aes(x=diagramData[1,1]-1,y=diagramData[1,2]-1,label="X1"))+
      geom_text(aes(x=diagramData[2,1]+1,y=diagramData[2,2]-1,label="X2"))+
      geom_text(aes(x=diagramData[3,1],y=diagramData[3,2]+1.2,label="Y"))+
      theme(legend.position="none")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

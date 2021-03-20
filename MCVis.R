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
                        step=1),
            
            
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
        
        #gets %distance from inputs and converts for display
        distDat<-params()
        
        
        x1ydist=(100-distDat$yX1Dist)
        x2ydist=(100-distDat$yX2Dist)
        x1x2dist=(100-distDat$x1X2Dist)
        
        #Placing X1 at a static location
        x1x <- 15
        x1y <- 20
        x1 <- data.frame("x"=x1x,"y"=x1y)
        
        #Placing X2 in a way which keeps it at a constant 90-degree angle from X1 (i.e. only horizontal movement)
        x2x <- x1$x + (2*(x1x2dist/100)*rUnit)
        x2y <- x1$y
        x2 <- data.frame("x"=x2x,"y"=x2y)
        
        #calculating y angle relative to X2's position at 90-degrees
        angleDat <- data.frame("x1x2"=(x1x2dist/100)*rUnit,"x1y"=(x1ydist/100)*rUnit,"x2y"=(x2ydist/100)*rUnit)
        yAngle <- (90*d2r)-acos(((angleDat$x1y^2)+(angleDat$x1x2^2)-(angleDat$x2y^2))/(2*(angleDat$x1y*angleDat$x1x2)))
        
        #Placing Y at the correct angle and distance from X1 as to maintain its distance with X2
        yx <- x1$x+(((2*x1ydist/100)*rUnit)*sin(yAngle))
        yy <- x1$y+(((2*x1ydist/100)*rUnit)*cos(yAngle))
        y <- data.frame("x" = yx, "y" = yy)

        df.venn <- data.frame(x = c(x1$x, y$x, x2$x),
                              y = c(x1$y, y$y, x2$y),
                              labels = c('X1', 'Y', 'X2'))
        return(df.venn)
    })
    
    output$distPlot <- renderPlot({
        diagramData<-makeCircles()
        ggplot(diagramData, aes(x0 = x, y0 = y, r = 1, fill = labels)) +
            geom_circle(alpha = .3, size = 1, colour = 'grey') +
            coord_fixed() +
            scale_x_continuous(limits=c(12,18))+
            scale_y_continuous(limits=c(18.9,22))+
            theme_void()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
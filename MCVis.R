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

GenParams <- function(yX1Dist,yX2Dist,degree){
    newDat <- data.frame(yX1Dist,yX2Dist,degree)
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
            sliderInput("degree",
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
        GenParams(input$yX1Dist,input$yX2Dist,input$degree)
    })
    makeCircles <- reactive({
        #converts degrees to radians for rotating X2's position
        d2r=0.0174533
        
        #gets %distance from inputs and converts for display
        distDat<-params()
        
        
        x1ydist=(100-distDat$yX1Dist)
        x2ydist=(100-distDat$yX2Dist)
        
        #these constants seem to be based on coordinates we're using
        x1radiusFactor=(x1ydist)/2335
        x2radiusFactor=(x2ydist)/1655
        
        #Generating values for Y position, creating a data frame
        yx <- 15
        yy <- 20 
        y<-data.frame("x" = yx,"y" = yy)
        
        #Generating values for X2 position, creating data frame
        x1x <- y$x-(sqrt(x1ydist^2)/2)*x1radiusFactor
        x1y <- y$y-(sqrt(x1ydist^2)/2)*x1radiusFactor
        x1 <- data.frame("x" = x1x, "y" = x1y)
        
        #165 is the starting degree around Y, and (60/100)*distDat$degree limits rotation from 0 overlap with X1 to perfect overlap with X1
        x2x <- y$x + ((sqrt(x2ydist^2)/2)*sin(d2r*(165+(60/100)*distDat$degree)))*x2radiusFactor
        x2y <- y$y + ((sqrt(x2ydist^2)/2)*cos(d2r*(165+(60/100)*distDat$degree)))*x2radiusFactor
        x2 <- data.frame("x" = x2x, "y" = x2y)
        #Plotting
        df.venn <- data.frame(x = c(x1$x, y$x, x2$x),
                              y = c(x1$y, y$y, x2$y),
                              labels = c('X1', 'Y', 'X2'))
        return(df.venn)
    })
    
    output$distPlot <- renderPlot({
        diagramData<-makeCircles()
        ggplot(diagramData, aes(x0 = x, y0 = y, r = 1.5, fill = labels)) +
            geom_circle(alpha = .3, size = 1, colour = 'grey') +
            coord_fixed() +
            scale_x_continuous(limits=c(11,17.5))+
            scale_y_continuous(limits=c(15,22))+
            theme_void()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
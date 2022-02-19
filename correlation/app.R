#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Correlation"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          
            
            
            ## Define the sample size
            sliderInput("sample_size",
                        h3("Sample Size:"),
                        min = 10,
                        max = 1000,
                        value = 50),
            ## Define the correlation value
            sliderInput("correlation",
                        h3("Correlation Value:"),
                        min = -1,
                        max = 1,
                        value = 0,step = 0.01),
            ## Display regression line
            checkboxInput('line', 
                          label = h3('Display Linear Line:'),
                          value = FALSE),
            
            ## Define Action Button
            actionButton("button","Calculate")
        ),
        
        
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    inform <- eventReactive(input$button, {
        
        
        ## 1) Get sample size, Correlation, display line
        sample_size = input$sample_size
        correlation = input$correlation
        line = input$line
        
        
        
        return(list(sample_size, correlation,line))
        
        
    })
    
    
    
    output$distPlot <- renderPlot({
        ## 0) Define bins button
        inform <- inform()
        ## 1) Gather sample size, Correlation, display line
        sample_size_button = inform$sample_size
        correlation_button = inform$correlation
        line_button = inform$line
        
        ## 2) Generate Data
        data = mvrnorm(n=sample_size_button, mu=c(0, 0), Sigma=matrix(c(1, 
                                                                      correlation_button,
                                                                      correlation_button,
                                                                      1), 
                                                                    nrow=2), 
                       empirical=TRUE)
        
        ## 3) Turn Data into data frame
        df = data.frame(X = data[, 1], Y = data[, 2] )
        
        
        
        ## 4) Display graph
        if(line_button == TRUE){ ## Display Line
            ggplot(df,aes(x = X, y = Y)) +
                geom_point() +
                geom_smooth(method='lm', formula= y~x,se = FALSE,size =2) +
                theme_bw()  
        } else {               ## No Line
            ggplot(df,aes(x = X, y = Y)) +
                geom_point() +
                theme_bw()
            
        }
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

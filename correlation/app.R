#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(MASS)
suppressPackageStartupMessages(library(tidyverse))
library(rmarkdown)
library(knitr)
library(pander)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Correlation"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          
            
            
            ## 1) SliderinputDefine the sample size
            sliderInput("sample_size",
                        h3("Sample Size:"),
                        min = 10,
                        max = 1000,
                        value = 50),
            
            
            ## 2) Sliderinput - Define the correlation value
            sliderInput("correlation",
                        h3("Correlation Value:"),
                        min = -1,
                        max = 1,
                        value = 0,step = 0.01),
            
            
            
            
            ## 3) Checkbox - Display regression line
            checkboxInput('line', 
                          label = h3('Display Linear Line:'),
                          value = FALSE),
            
            
            
            ## 4) Button - Define Action Button
            actionButton("button","Calculate")
        ),
        
        
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("regression_line_plot"),
            br(),
            uiOutput("sums_of_square")
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
        
        ## 2) Generate Data
        data = mvrnorm(n=sample_size, 
                       mu=c(0, 0), 
                       Sigma=matrix(c(1, correlation, 
                                      correlation, 1),nrow=2), 
                       empirical=TRUE)
        
        ## 3) Turn Data into data frame
        df = data.frame(X = data[, 1], Y = data[, 2] )
        
        ## 4) Run Model
        slr_model = lm(Y ~ X, data = df)
        
        ## 5) Define expected response variable values, observed response variables, sample mean of the response variable
        y_hat = predict(slr_model)
        y_obs = df$Y
        y_mean = mean(y_obs)
        
        ## 6) Define Sums of Square: Total, Error, & Treatment
        SS_Total = sum((y_obs - y_mean)^2)
        SS_Error = sum((y_obs - y_hat)^2)
        SS_Treatment = sum((y_hat - y_mean)^2)
        
        
        return(list(df = df,line = line,
                    SS_Total = SS_Total,
                    SS_Treatment = SS_Treatment,
                    SS_Error = SS_Error
                    ))
        
    })
    
    
    
    output$regression_line_plot <- renderPlot({
        ## 0) Define bins button
        inform <- inform()
        ## 1) Gather df & display line
        df = inform$df
        line_button = inform$line
        
    
        
        
        
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
    
    
    
    output$sums_of_square <- renderUI({
        
        
        ## 0) Define bins button
        inform <- inform()
        ## 1) Gather SS: Total, Treatment & Error
        SS_Total = inform$SS_Total
        SS_Treatment = inform$SS_Treatment
        SS_Error = inform$SS_Error
        
        
        
    
            withMathJax(
                paste0("\\(\\sum_{i=1}^n (y_i - \\bar{y})^2 =\\) ", round(SS_Total, 3)),
                br(),
                paste0("\\(\\sum_{i=1}^n (\\hat{y}_i - \\bar{y})^2 =\\) ", round(SS_Treatment, 3)),
                br(),
                paste0("\\(\\sum_{i=1}^n (y_i - \\hat{y})^2 =\\) ", round(SS_Error, 3)),
                br()
            )
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

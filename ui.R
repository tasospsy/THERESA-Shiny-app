## Theresa - UI
## Thesis Project - FlexCAT
## Tasos Psychogyiopoulos
## c.27/05/2022

library(shiny)
library(bslib)
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  theme = bs_theme(version = 5, bootswatch = "journal"),
  
  # Application title
  titlePanel("THERESA - THEsis REsults Application"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = 'n',
                         label = 'Sample Size (N)',
                         choices = c(500, 1000, 2000, 5000),
                         selected = c(500)),
      checkboxGroupInput(inputId = 'j',
                         label = 'Number of Items (J)',
                         choices = c('J = 7', 'J = 15'),
                         selected = c('J = 7')
      ),
      checkboxGroupInput(inputId = 'k',
                         label = 'Number of True Classes (K)',
                         choices = c('K = 4', 'K = 8', 'K = 12'),
                         selected = c('K = 4')
      ),
      checkboxGroupInput(inputId = 'ic',
                         label = 'Information Criterion (IC)',
                         choices = c('AIC', 'AIC3', 'BIC', 'aBIC' ),
                         selected = c('AIC', 'AIC3', 'BIC', 'aBIC' )
      ),
      width = 2
    ),
    mainPanel(
      plotlyOutput("p1",width = "100%"),
      plotlyOutput("p2",width = "100%")
      
    )
  )
))

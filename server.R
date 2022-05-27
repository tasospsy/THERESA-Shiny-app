## Theresa - Server
## Thesis Project - FlexCAT
## Tasos Psychogyiopoulos
## c.27/05/2022

library(tidyverse)
library(shiny)
library(plotly)

## load files and necessary objects
source(url("https://raw.githubusercontent.com/tasospsy/THERESA-Shiny-app/main/prep.R"))
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$p1 <- renderPlotly({
    
    Nf <- input$n
    Jf <- input$j
    Kf <- input$k
    ICf <- input$ic
    
    ggplotly(
      count.ICs.K  %>% 
        filter(N %in% Nf) %>% 
        filter(IC %in% ICf) %>% 
        filter(K %in% Kf) %>% 
        filter(J %in% Jf) %>% 
        mutate(t.K = as.integer(stringr::str_extract(K, "\\d+"))) %>% 
        ggplot(aes(x= est.K, y = IC, fill = IC, size = n, shape = IC)) + 
        geom_point(alpha=0.5, shape = 22, color="black") + 
        scale_fill_manual(values = colors)+
        geom_vline(aes(xintercept= t.K), linetype="dotted", 
                   color = "black", size=.4)+
        facet_grid(fct_relevel(N,'500','1000','2000', '5000') ~ 
                     fct_relevel(J, 'J = 7', 'J = 15') +
                     fct_relevel(K, 'K = 4', 'K = 8', 'K = 12'),
                   scales = 'free_x') + 
        labs(title = "",
             x = "Number of estimated classes",
             y = "Information Criterion (IC)")+
        theme_bw() + theme1 +
        theme(legend.position='none',
              axis.text.x = element_text(size=11, color = "black")),
      tooltip = c('est.K', 'n'))
  })
  
  output$p2 <- renderPlotly({
    Nf <- input$n
    Jf <- input$j
    Kf <- input$k
    ICf <- input$ic
    
    ggplotly(
      KL2 %>% 
        mutate(J = case_when(J== 7 ~ 'J = 7',
                             J== 15 ~ 'J = 15'),
               K = case_when(K == 4 ~  'K = 4',
                             K == 8 ~  'K = 8',
                             K == 12 ~ 'K = 12')) %>%
        mutate(t.K = as.integer(stringr::str_extract(K, "\\d+"))) %>% 
        filter(N %in% Nf) %>% 
        filter(K %in% Kf) %>% 
        filter(J %in% Jf) %>% 
        ggplot() +
        ## Dotted line for true K
        geom_vline(aes(xintercept= t.K), linetype="dotted", 
                   color = "black", size=.4) +
        ## Π+ smooth line and points
        geom_point(aes(x = est.K, y = KL.P), 
                   size = .6, color = 'lightblue',alpha = .4) +
        geom_smooth(aes(x = est.K, y = KL.P), method = 'loess', 
                    color = 'darkblue', size = .8) +
        ## Π smooth line and points
        geom_point(aes(x = est.K, y = KL.Pp), 
                   size = .6, color = 'pink', alpha = .5) +
        geom_smooth(aes(x = est.K, y = KL.Pp), method = 'loess', 
                    color = 'darkred', size = .8) +
        ## Line and point for ICs: most likely picked model
        #geom_vline(data = count.ICs.K %>%
        #               filter(IC %in% ICf) %>% 
        #               filter(n == max(n)),
        #           mapping = aes(xintercept= est.K, color = IC), 
        #           size=.6, alpha = .6, linetype = 'solid') +
        #geom_point(data = count.ICs.K%>% 
        #               filter(IC %in% ICf) %>% 
        #               filter(n == max(n)),
        #           mapping = aes(x= est.K, y = -.05, color = IC, shape = IC), 
        #           size=3, alpha = .5) +
      
      scale_color_manual(values = colors)+
        facet_grid(fct_relevel(N,'500','1000','2000', '5000') ~ 
                     fct_relevel(J, 'J = 7', 'J = 15') +
                     fct_relevel(K, 'K = 4', 'K = 8', 'K = 12'),
                   scales = 'free_x') + 
        labs(title = "<span style = 'color:darkblue;font-size:19px'> **π Vs π&#770;**</span> & 
       <span style = 'color:darkred;font-size:19px'>**π<sub>+</sub> Vs π&#770;<sub>+</sub>**</span>",
       x = "Number of estimated classes",
       y = "Kullback - Leibler distance") +
        theme_bw() + theme1 +
        theme(axis.text.x = element_text(size=11, color = "black"), 
              #legend.position = 'none'
        )
    )
    
  })
  
})

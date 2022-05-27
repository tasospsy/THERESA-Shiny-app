## Theresa - Perp
## Thesis Project - FlexCAT
## Tasos Psychogyiopoulos
## c.27/05/2022

## Load files 
load(url("https://github.com/tasospsy/THERESA-Shiny-app/blob/main/data/KL2.Rdata?raw=true"))
load(url("https://github.com/tasospsy/THERESA-Shiny-app/blob/main/data/out-td.Rdata?raw=true"))



## Visualization manual theme
colors <- RColorBrewer::brewer.pal(9, 'Paired')[c(3,4,7,8)]
theme1 <- theme(plot.background = element_rect(fill = "white", color = NA), #background color
                text = element_text(family = "Courier New", color = "black"), # color of all text in the plot 
                #plot.title = element_text(hjust = 0.5, color = "black", size = 12), # specs of the title
                strip.text = element_text(colour = "black", size = 14), # specs of the text inside plot
                panel.grid.major.x = element_line(size = .5), # change the grid layout
                panel.grid.major.y = element_line(size = .5), # change the grid layout
                panel.grid.minor.x = element_blank(), # remove the grid layout
                panel.grid.minor.y = element_blank(), # remove the grid layout
                axis.text=element_text(size=10, color = "black"), # specs of the text in axis
                axis.text.x = element_blank(),
                legend.position = "bottom", # legend down
                #legend.title = element_blank(), # remove legend title,
                legend.text = element_text(colour = "black", size = 9),
                #plot.title = element_markdown(size = 12,hjust = 0,lineheight = 1, 
                                              #color = "black", family = 'mono'),
                strip.background =element_rect(fill="grey100")
                )

## which is the maximum percentage of estimated K that is 
## picked by each IC, within each condition of the 24?
count.ICs.K <- out.td %>% 
  rowwise() %>% mutate_at(vars(AIC,AIC3,BIC,aBIC),  ~.x + K)  %>% 
  group_by(TrueMod, N, K, J) %>% 
  pivot_longer(cols = c(AIC,AIC3,BIC,aBIC), 
               names_to = 'IC', 
               values_to = 'est.K') %>% 
  mutate(IC = fct_relevel(IC,'AIC', 'AIC3', 'BIC', 'aBIC')) %>% 
  group_by(TrueMod, N, K, J,IC) %>% 
  count(est.K) %>% 
  mutate(J = case_when(J== 7 ~ 'J = 7',
                       J== 15 ~ 'J = 15'),
         K = case_when(K == 4 ~  'K = 4',
                       K == 8 ~  'K = 8',
                       K == 12 ~ 'K = 12'))
                
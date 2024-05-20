###NEED TO UPDATE INFO HERE


# Clear the workspace
rm( list = ls() )


library( dplyr )     # for working with the data
library( reshape2 )  # for reworking the data
library( ggplot2 )   # for plotting
library( gridExtra ) # for plotting grids
library( grid )      # for plotting titles on the grid


set.seed( 2468 )

n <- 100 

noConDat <- data.frame(
  knowledge = runif( n = n, min = .00 , max = 0.30 ),
  culture = rep( 1, n )
)

conDat <- data.frame(
  knowledge = runif( n = n, min = .80 , max = 0.99 ),
  culture = rep( 1, n )
)

hetConDat <- data.frame(
  knowledge = c( runif( n = n/2, min = .80 , max = 0.99 ), runif( n = n/2, min = .80 , max = 0.99 ) ) ,
  culture = c( rep( 1, n/2 ), rep( 2, n/2 ) )
)

hetConNoDat <- data.frame(
  knowledge = c( runif( n = n/2, min = .80 , max = 0.99 ), runif( n = n/2, min = .00 , max = 0.30 ) ) ,
  culture = c( rep( 1, n/2 ), rep( 2, n/2 ) )
)


# Add a new variable that ranks responses within each group
noConDat <- noConDat %>%
  arrange( culture, knowledge ) %>%
  mutate( rank_knowledge = row_number() )

conDat <- conDat %>%
  arrange( culture, knowledge ) %>%
  mutate( rank_knowledge = row_number() )

hetConDat <- hetConDat %>%
  arrange( culture, knowledge ) %>%
  mutate( rank_knowledge = row_number() )

hetConNoDat <- hetConNoDat %>%
  arrange( culture, knowledge ) %>%
  mutate( rank_knowledge = row_number() )




# Create the scatterplots
# ideally this a function that loops through labels and the data

noConPlot <- ggplot( noConDat,
                         aes( x = rank_knowledge, 
                              y = knowledge, 
                              color = as.factor( culture ) ) ) +
  geom_point() + 
  labs( title = "Noncoherent",
        x = "Respondents",
        y = "Knowledge") +
  theme_minimal() +
  theme( legend.position = "none",
         axis.text.x = element_blank() ) +
  guides( color = guide_legend( title = "Group" ) ) + 
  scale_y_continuous( limits = c( 0, 1 ) )
print( noConPlot )


ConPlot <- ggplot( conDat,
                     aes( x = rank_knowledge, 
                          y = knowledge, 
                          color = as.factor( culture ) ) ) +
  geom_point() + 
  labs( title = "Monocentric",
        x = "Respondents",
        y = "Knowledge") +
  theme_minimal() +
  theme( legend.position = "none",
         axis.text.x = element_blank() ) +
  guides( color = guide_legend( title = "Group" ) ) + 
  scale_y_continuous( limits = c( 0, 1 ) )
print( ConPlot )


hetConPlot <- ggplot( hetConDat,
                   aes( x = rank_knowledge, 
                        y = knowledge, 
                        color = as.factor( culture ) ) ) +
  geom_point() + 
  labs( title = "Multicentric",
        x = "Respondents",
        y = "Knowledge") +
  theme_minimal() +
  theme( legend.position = "none",
         axis.text.x = element_blank() ) +
  guides( color = guide_legend( title = "Group" ) ) + 
  scale_y_continuous( limits = c( 0, 1 ) )
print( hetConPlot )


hetConNoPlot <- ggplot( hetConNoDat,
                      aes( x = rank_knowledge, 
                           y = knowledge, 
                           color = as.factor( culture ) ) ) +
  geom_point() + 
  labs( title = "Multicentric",
        x = "Respondents",
        y = "Knowledge") +
  theme_minimal() +
  theme( legend.position = "none",
         axis.text.x = element_blank(), 
         plot.margin = unit( c( 1, 1, 1, 1 ), "cm" ),
         plot.title = element_text( hjust = 0.5 ) ) +
  guides( color = guide_legend( title = "Group" ) ) + 
  scale_y_continuous( limits = c( 0, 1 ) )
print( hetConNoPlot )

grid.arrange( 
  noConPlot, ConPlot, hetConPlot, hetConNoPlot, 
  nrow = 2, ncol = 2
  )





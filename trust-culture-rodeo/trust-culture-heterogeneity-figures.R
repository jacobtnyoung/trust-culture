

####CLEAN ALL THIS UP!!!!



# Clear the workspace
rm( list = ls() )

library( here )      # for the directory
library( dplyr )     # for working with the data
library( reshape2 )  # for reworking the data
library( ggplot2 )   # for plotting
library( gridExtra ) # for plotting grids
library( grid )      # for plotting titles on the grid

# set seed to reproduce same objects
set.seed( 2468 )


# ================================================================== #
# Function to create the agreement matrix ----
# The equation comes from pg. 65 of Purzycki, B. G., & Jamieson-Lane, A. (2016). AnthroTools. Cross-Cultural Research, 51(1), 51-74. doi:10.1177/1069397116680352.

DiscountedAgreementMatrix <-
  function( response.mat, n.responses ){
    discounted.matrix= matrix( 0, nrow( response.mat ),nrow( response.mat ) )
    for( i in 1:nrow( discounted.matrix ) ){
      for( j in 1:ncol( discounted.matrix ) ){
        discounted.matrix[i,j] <- mean( response.mat[i,] == response.mat[j,], na.rm = TRUE ) # Create the average number of items i and j agree on
      }             
    }
    discounted.matrix <- ( discounted.matrix*n.responses -1 ) / (n.responses-1 ) # Correct the average agreement for guessing
    diag( discounted.matrix ) <- 0 # set the diagonal to zero
    discounted.matrix[is.finite(discounted.matrix) == FALSE] <- 0 # adjust Inf values to zero (if any)
    return( discounted.matrix ) 
  }


# Create data for the plot.
dat.for.plot <- function( cormat ){
  dd <- as.dist( ( 1-cormat )/2 ) # Use correlation between variables as distance
  hc <- hclust( dd )
  cormat <-cormat[hc$order, hc$order]
  cormat[lower.tri( cormat )] <- NA # set the lower part of the triangle as missing (redundant information)
  cor.dat <- melt( cormat, na.rm = TRUE ) # create the dataframe
  return( cor.dat )
}


# ================================================================== #
# Create the data objects ----

nResp <- 100
nVars <- 50 

# build the function to create the response matrices
build.responses <- function( nResp, nVars, probValue, nresponses ){
  
  # create the blank object
  Response <- matrix( NA, nrow = nResp, ncol = nVars )
  
  # loop through to create the responses
  for( i in 1: nVars ){
    Response[,i] <- rbinom( nResp, 1, prob = probValue )
  }
  
  # create the agreement matrix
  Agreement <- DiscountedAgreementMatrix( Response, nresponses )
  
  # Assign the agreement matrix to the object to plot
  AgreementDat <- dat.for.plot( Agreement )
  
  # return the agreement data as a data frame to plot
  return( AgreementDat )
  
}

noConAgreementDat <- build.responses( nResp, nVars, probValue = 0.5, nresponses = 2 )
ConAgreementDat   <- build.responses( nResp, nVars, probValue = 0.8, nresponses = 2 )


# build the function to create the response matrices
build.het.responses <- function( nResp, nVars, probValue1, probValue2, nresponses ){
  
  # create the blank object
  Response <- matrix( NA, nrow = nResp, ncol = nVars )
  
  # loop through to create the responses
  for( i in 1: nResp / 2 ){
    Response[i,] <- rbinom( nResp / 2, 1, prob = probValue1 )
    Response[i + nResp / 2,] <- rbinom( nResp / 2, 1, prob = probValue2 )
  }
  
  # create the agreement matrix
  Agreement <- DiscountedAgreementMatrix( Response, nresponses )
  
  # Assign the agreement matrix to the object to plot
  AgreementDat <- dat.for.plot( Agreement )
  
  # return the agreement data as a data frame to plot
  return( AgreementDat )
  
}

HetConAgreementDat   <- build.het.responses( nResp, nVars, probValue1 = 0.8, probValue2 = 0.2, nresponses = 2 )
HetConNoAgreementDat <- build.het.responses( nResp, nVars, probValue1 = 0.8, probValue2 = 0.5, nresponses = 2 )


# ================================================================== #
# Create the data objects ----

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



# ================================================================== #
# Plotting results ----


datToPlot <- list( noConAgreementDat, ConAgreementDat, HetConAgreementDat, HetConNoAgreementDat )

titlesPlot <- c(
  "Noncoherent",
  "Monocentric",
  "Multicentric",
  "Multicentric"
)

plotList <- list()

for( i in 1: length( datToPlot ) ) {
  
  plotList[[i]] <- ggplot( data = datToPlot[[i]], aes( Var2, Var1, fill = value ) ) +
    geom_tile( color = "white" ) +
    scale_fill_gradient2( low = "blue", high = "red", mid = "white", 
                          midpoint = 0, limit = c( -1,1 ), space = "Lab", 
                          name="Agreement" ) +
    theme_minimal() + 
    coord_fixed() + 
    theme(
      axis.title.x = element_blank(), axis.title.y = element_blank(),
      axis.text.x=element_blank(), axis.text.y=element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      legend.justification = c(1, 0),
      legend.position = c(0.6, 0.7),
      legend.direction = "horizontal") +
    guides(fill = guide_colorbar( barwidth = 7, barheight = 1,
                                  title.position = "top", title.hjust = 0.5) ) +
    ggtitle( titlesPlot[i] )
  #print( plotList[[i]] )
  
}


grid.arrange( 
  plotList[[1]],plotList[[2]], plotList[[3]], plotList[[4]],
  nrow = 2, ncol = 2
)


# export the image
pdf( file = here( "trust-culture-figures/hypothetical.agreement.pdf" ) )

grid.arrange( 
  plotList[[1]],plotList[[2]], plotList[[3]], plotList[[4]],
  nrow = 2, ncol = 2
)

dev.off()




# ================================================================== #
# Plots ----




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



grid.arrange( 
  noConPlot, ConPlot, hetConPlot, hetConNoPlot, 
  nrow = 2, ncol = 2
)


# export the image
pdf( file = here( "trust-culture-figures/hypothetical.knowledge.pdf" ) )

grid.arrange( 
  noConPlot, ConPlot, hetConPlot, hetConNoPlot, 
  nrow = 2, ncol = 2
)

dev.off()





grid.arrange( 
  plotList[[1]], noConPlot,
  plotList[[2]], ConPlot,
  plotList[[3]], hetConPlot,
  plotList[[4]], hetConNoPlot,
  nrow = 4, ncol = 2
)







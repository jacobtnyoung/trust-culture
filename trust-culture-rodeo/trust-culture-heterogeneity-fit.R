# ================================================================== #
# This syntax file assess model fit for models estimated
# in the trust-culture-heterogeneity-estimation.R file


# ================================================================== #
# Setup ----

# Clear the workspace
rm( list = ls() )

# Load the libraries used
library( CCTpack )   # for the cultural consensus models
library( here )      # for calling local directory
library( dplyr )     # for working with the data
library( reshape2 )  # for reworking the data
library( ggplot2 )   # for plotting the agreement matrix
library( gridExtra ) # for plotting grids
library( grid )      # for plotting titles on the grid


# ================================================================== #
# load the trust variables ----

# load the data
trust.vars <- readRDS( file = here( "trust-culture-rodeo/trust.fit.data.rds" ) )


# ================================================================== #
# create the agreement matrix and export it ----

# Function to create the agreement matrix.
# The equation comes from pg. 65 of Purzycki, B. G., & Jamieson-Lane, A. (2016). 
# AnthroTools. Cross-Cultural Research, 51(1), 51-74. doi:10.1177/1069397116680352.

DiscountedAgreementMatrix <-
  function( response.mat, n.responses ){
    discounted.matrix= matrix( 0, nrow( response.mat ),nrow( response.mat ) )
    for( i in 1:nrow( discounted.matrix ) ){
      for( j in 1:ncol( discounted.matrix ) ){
        discounted.matrix[i,j] <- mean( response.mat[i,] == response.mat[j,], na.rm = TRUE ) # Create the average number of items i and j agree on.
      }             
    }
    discounted.matrix <- ( discounted.matrix*n.responses -1 ) / (n.responses-1 ) # Correct the average agreement for guessing.
    diag( discounted.matrix ) <- 0 # set the diagonal to zero.
    discounted.matrix[is.finite(discounted.matrix) == FALSE] <- 0 # adjust Inf values to zero (if any).
    return( discounted.matrix ) 
  }

trust.agreement <- DiscountedAgreementMatrix( trust.vars[,-1], 2 )
mean( trust.agreement )


dat.for.plot <- function( cormat ){
  dd <- as.dist( ( 1-cormat )/2 ) # Use correlation between variables as distance.
  hc <- hclust( dd )
  cormat <-cormat[hc$order, hc$order]
  cormat[lower.tri( cormat )] <- NA # set the lower part of the triangle as missing (redundant information).
  cor.dat <- melt( cormat, na.rm = TRUE ) # create the dataframe.
  return( cor.dat )
}

# assign the agreement matrix to the object to plot.
trust.agreement.dat <- dat.for.plot( trust.agreement )

# Plot for trust.
ggheatmap.T <- ggplot( data = trust.agreement.dat, aes( Var2, Var1, fill = value ) ) +
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
  ggtitle( "Plot of Agreement for Trust Items" )
print( ggheatmap.T )


# ================================================================== #
# examine the scree plot and export it ----

# show the scree plot
trust.plot   <- cctscree( trust.vars[,-1], polych = FALSE ) 


# ================================================================== #
# Load the estimated models ----

# load the object and set the name
trust.fit.2.diff <- load( here( "trust-culture-rodeo/trust.fit.2.diff.Rdata" ) )
trust.fit.2.diff <- cctfit

# plot the posterior predictive model checks
cctppc( trust.fit.2.diff )

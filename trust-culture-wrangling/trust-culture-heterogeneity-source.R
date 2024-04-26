# ================================================================== #
# PAPER TITLE
# ================================================================== #

# This syntax file cleans the items from the trust instrument 
# of the PAR survey.

# This file is sourced in the trust-culture-heterogeneity-analysis.R file.


# ================================================================== #
# Setup ----

# Clear the workspace
rm( list = ls() )

# Load the libraries used
library( here ) # for referencing the local directory


# ================================================================== #
# Load the data ----

dat <- read.csv(
  here( "trust-culture-wrangling/trust-culture-trust-items.csv" ),
  as.is = TRUE,
  header = TRUE,
  stringsAsFactors = FALSE
)
  

# ================================================================== #
# trust variables ----

# the trust items are S3SS1_1-S3SS1_15
trust.vars <- as.matrix( dat[,
                             which( colnames( dat ) == ( "S3SS1_1" ) ):
                               which( colnames( dat ) == ( "S3SS1_15" ) )
] )

# add the ids
trust.vars <- cbind( dat$id, trust.vars )

# Function to replace the missing values
NA.replace <- function( input.dat, pdist ){
  set.seed( 12345 )
  for( i in 1:nrow( input.dat ) ){
    for( j in 1:ncol( input.dat ) ){
      if( is.na( input.dat[i,j] == TRUE ) )
        input.dat[i,j] <- pdist
    }
  }
  return( input.dat )
}

# Show there are 67 missing values over the 15 items
table( is.na( trust.vars[,-1] ) )

# Run the function to replace the missing values
trust.vars.na   <- NA.replace( as.matrix( trust.vars[,-1] ), rbinom( n = 1, size = 1, prob = 0.5 ) )

# Add back the ids
trust.vars.na <- cbind( dat$id, trust.vars.na )

# Drop 2 cases that have repeated responses
trust.vars.na <- trust.vars.na[ 
  which( apply( trust.vars.na[,-1], 1, function( x ) length( unique( x ) ) ) != 1 ), ]
trust.vars.na <- na.omit( trust.vars.na )

# Show the scree plot
trust.plot   <- cctscree( trust.vars.na[,-1], polych = FALSE ) 


# ================================================================== #
# Estimate models ----

!!!HERE: Need to fill this in as the models are estimated

!!!When you are happy with the model setup, put that here








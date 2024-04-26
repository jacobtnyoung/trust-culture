# ================================================================== #
# PAPER TITLE
# ================================================================== #

# This syntax file cleans the items from the trust instrument 
# of the PAR survey.

# Then, it executes the heterogeneity analysis.


!!!HERE: trying to figure out how best to set this up!!!


Source the file and then update this with the models to get it to work right.  
  


# ================================================================== #
# Setup ----

# Clear the workspace
rm( list = ls() )

# Load the libraries used
library( here ) # for referencing the local directory
library( )

# ================================================================== #
# Load the data ----

dat <- read.csv(
  here( "trust-culture-wrangling/trust-culture-trust-items.csv" ),
  as.is = TRUE,
  header = TRUE,
  stringsAsFactors = FALSE
)
  

# ================================================================== #
# Trust variables ----

# Extract the trust variables
# S3SS1_1-S3SS1_15
trust.vars <- as.matrix( dat[,
                             which( colnames( dat ) == ( "S3SS1_1" ) ):
                               which( colnames( dat ) == ( "S3SS1_15" ) )
] )


#!!!HERE: need to clean up and decide whether to use this

### !!! NEED TO DECIDE WHETHER TO DO THIS OR NOT!!!

## keep variables that are worded well
#vars.keep <- c( "S3SS1_1", "S3SS1_3", "S3SS1_4", "S3SS1_7", "S3SS1_9", "S3SS1_10", "S3SS1_11", "S3SS1_12", "S3SS1_14" )
#trust.vars2 <- ( trust.vars[, which( colnames( trust.vars ) %in%  vars.keep )  ] )
#trust.vars  <- trust.vars2


# Clean up missing values and examine descriptive statistics
#
## Replace the missing values
#NA.replace <- function( input.dat, pdist ){
#  set.seed( 12345 )
#  for( i in 1:nrow( input.dat ) ){
#    for( j in 1:ncol( input.dat ) ){
#      if( is.na( input.dat[i,j] == TRUE ) )
#        input.dat[i,j] <- pdist
#    }
#  }
#  return( input.dat )
#}

## Run the function to replace the missing values
## replace 67 missing values: table( is.na( trust.vars ) )
#trust.dat   <- NA.replace( trust.vars, round( runif( n = 1, min = 0, max = 1 ), 0 ) )
#apply( trust.dat, 2, mean )
#apply( trust.dat, 2, sd )
#mean( apply( trust.dat, 2, mean ) )
#sd( apply( trust.dat, 2, mean ) )




# ================================================================== #
# END OF FILE
# ================================================================== #
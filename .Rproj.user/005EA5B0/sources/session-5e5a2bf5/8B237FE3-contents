# ================================================================== #
# This file prepares the relational health and psychological
# safety variables for analysis with the knowledge scores


# ================================================================== #
# Setup ----

# Clear the workspace
rm( list = ls() )

# Load the libraries used
library( here )     # for calling local directory
library( dplyr )    # for working with the data
library( psych )    # for reliability analysis


# ================================================================== #
# Load the data ----

dat <- read.csv(
  here( "trust-culture-wrangling/trust-culture-rhps-cntrls-items.csv" ),
  as.is = TRUE,
  header = TRUE,
  stringsAsFactors = FALSE
)


# ================================================================== #
# assign the variables to matrices ----

# extract the relational health variables, S3SS2_1-S3SS2_14
rh.vars <- as.matrix( dat[,
                             which( colnames( dat ) == ( "S3SS2_1" ) ):
                               which( colnames( dat ) == ( "S3SS2_14" ) )
] )

# extract the psychological safety variables, S3SS3_1-S3SS3_7
ps.vars <- as.matrix( dat[,
                          which( colnames( dat ) == ( "S3SS3_1" ) ):
                            which( colnames( dat ) == ( "S3SS3_7" ) )
] )


# ================================================================== #
# missing data ----

# function to replace the missing values
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


### relational health

# replace 15 missing values
table( is.na( rh.vars ) )

# execute the function
rh.vars   <- NA.replace( rh.vars, round( runif( n = 1, min = 1, max = 5 ), 0 ) )

# examine reliability
alpha( rh.vars )

# create the scale
rh.dat <- apply( rh.vars, 1, mean )

### psychological safety

# replace 29 missing values
table( is.na( ps.vars ) )

# execute the function
ps.vars   <- NA.replace( ps.vars, round( runif( n = 1, min = 1, max = 5 ), 0 ) )

# examine reliability
alpha( ps.vars )

# drop the problematic items
ps.vars <- ps.vars[, - c( 2,5 ) ]

# examine reliability again
alpha( ps.vars )

# create the scale
ps.dat <- apply( ps.vars, 1, mean )


# ================================================================== #
# prep data for merging with the knowledge data ----

model.dat <- data.frame(
  assignmentID = seq( 1:200 ),
  ID = dat$id,
  rh = rh.dat,
  ps = ps.dat,
  interviewer = dat$InterviewerType,
  randomized = dat$Randomize,
  age = dat$S4Q1,
  white = dat$white,  
  black = dat$black,
  hispanic = dat$hispanic,
  timein_yrs = dat$timein_yrs
)


# ================================================================== #
# save the file for calling in the analysis ----

saveRDS( model.dat, paste( here(), "/trust-culture-rodeo/trust.data", ".rds", sep = "" ) )  

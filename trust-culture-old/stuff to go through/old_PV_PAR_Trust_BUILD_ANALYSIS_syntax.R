# ################################################################## #
# Perryville PAR Trust.
# ################################################################## #

# This syntax file examines the trust items from section 3 of the instrument.

# ================================================================== #
# Clear the workspace and load the libraries.

rm( list = ls() )

# ================================================================== #
# Load the data.

library( haven )
setwd( "/Users/jyoung20/Dropbox (ASU)/Perryville PAR/" )
dat <- as.data.frame( read_dta( "PV PAR Interviews Full_CLEANED.dta" ) )

# Extract the trust variables.
# S3SS1_1-S3SS1_15.
trust.vars <- as.matrix( dat[,
                  which( colnames( dat ) == ( "S3SS1_1" ) ):
                    which( colnames( dat ) == ( "S3SS1_15" ) )
] )

# drop S3SS1_5 due to insufficient variance.
vars.keep <- c( "S3SS1_1", "S3SS1_3", "S3SS1_4", "S3SS1_7", "S3SS1_9", "S3SS1_10", "S3SS1_11", "S3SS1_12", "S3SS1_14" )
trust.vars2 <- ( trust.vars[, which( colnames( trust.vars ) %in%  vars.keep )  ] )
trust.vars  <- trust.vars2

# drop the items that are not worded well.
#trust.vars2 <- trust.vars[, which( colnames( trust.vars ) != ( "S3SS1_5" ) )]
#trust.vars  <- trust.vars2

# Extract the relational health variables.
# S3SS2_1-S3SS2_14.
rh.vars <- as.matrix( dat[,
                             which( colnames( dat ) == ( "S3SS2_1" ) ):
                               which( colnames( dat ) == ( "S3SS2_14" ) )
] )

# Extract the psychological safety variables.
# S3SS3_1-S3SS3_7.
ps.vars <- as.matrix( dat[,
                          which( colnames( dat ) == ( "S3SS3_1" ) ):
                            which( colnames( dat ) == ( "S3SS3_7" ) )
] )


# ================================================================== #
# Trust variables.

# Replace the missing values.
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
  
# Run the function to replace the missing values.
# replace 67 missing values: table( is.na( trust.vars ) )
trust.dat   <- NA.replace( trust.vars, round( runif( n = 1, min = 0, max = 1 ), 0 ) )
apply( trust.dat, 2, mean )
apply( trust.dat, 2, sd )
mean( apply( trust.dat, 2, mean ) )
sd( apply( trust.dat, 2, mean ) )

# replace 15 missing values: table( is.na( rh.vars ) )
rh.dat   <- NA.replace( rh.vars, round( runif( n = 1, min = 1, max = 5 ), 0 ) )
apply( rh.dat, 2, mean )
apply( rh.dat, 2, sd )
mean( apply( rh.dat, 2, mean ) )
sd( apply( rh.dat, 2, mean ) )

# replace 15 missing values: table( is.na( rh.vars ) )
ps.dat   <- NA.replace( ps.vars, round( runif( n = 1, min = 1, max = 5 ), 0 ) )
apply( ps.dat, 2, mean )
apply( ps.dat, 2, sd )
mean( apply( ps.dat, 2, mean ) )
sd( apply( ps.dat, 2, mean ) )



# create an assignment variable.
trust.dat.r <- cbind( seq( 1:200 ), trust.dat  )
colnames( trust.dat.r ) <- c("id", colnames( trust.dat ) )
trust.dat2.r <- trust.dat.r[which( apply( trust.dat.r[,-1], 1, function(x) length( unique( x ) ) ) != 1 ),]  
use.ids <- trust.dat2.r[,1]

temp.group <- cctsubj( trust.group2 )
temp.group <- cbind( temp.group$group_Om, temp.group$comp_th )
temp.group <- cbind( use.ids, temp.group )
colnames( temp.group ) <- c( "use.ids", "group", "comp2")

comp.data <- cbind( seq( 1:200 ), trust.model$origCompetence  )
colnames( comp.data ) <- c( "use.ids", "comp" )

test <- merge( temp.group, comp.data, by.x = "use.ids", all = TRUE )

index <- test$group == 1
a <- test[ index == TRUE, ]
mean( a$comp, na.rm = TRUE )
mean( a$comp2, na.rm = TRUE )
plot( a$comp, a$comp2 )

# try the model again.
trust.dat.redo <- cbind( seq( 1:200 ), trust.dat  )
trust.dat.redo <- trust.dat.redo[index == TRUE, ]
index <- complete.cases( trust.dat.redo )
trust.dat.redo <- trust.dat.redo[index == TRUE, ]
trust.dat.redo <- trust.dat.redo[,-1]

trust.model2 <- ConsensusPipeline( SurveyResults = trust.dat.redo, numQ = 2 )
trust.model2
summary( trust.model2$origCompetence )

!so this is running the model on the 90 cases in 1 group.


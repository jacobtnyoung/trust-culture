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


# ================================================================== #
# Function to create the agreement matrix.
# The equation comes from pg. 65 of Purzycki, B. G., & Jamieson-Lane, A. (2016). AnthroTools. Cross-Cultural Research, 51(1), 51-74. doi:10.1177/1069397116680352.

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

trust.agreement <- DiscountedAgreementMatrix( trust.dat, 2 )
mean( trust.agreement )

rh.agreement <- DiscountedAgreementMatrix( rh.dat, 5 )
mean( rh.agreement )


# ================================================================== #
# Plotting results.

library( reshape2 )
library( ggplot2 )

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
# CCA Models.

#library("devtools")
#install_github("alastair-JL/AnthroTools") 
# https://github.com/alastair-JL/AnthroTools

library( AnthroTools )

trust.model <- ConsensusPipeline( SurveyResults = trust.dat, numQ = 2 )
trust.model
summary( trust.model$origCompetence )
length( which( trust.model$origCompetence > 0.7 ) ) / 200
hist( trust.model$origCompetence )

rh.model <- ConsensusPipeline( SurveyResults = rh.dat, numQ = 5 )
rh.model
summary( rh.model$origCompetence )
length( which( rh.model$origCompetence > 0.7 ) ) / 200

# Histogram of the competencies
hist( 
  trust.model$origCompetence,
  xlim=c( -1,1 ),
  breaks = 12,
  main = "Distribution of Cultural\n Embeddedness Scores",
  xlab = "Cultural Embeddedness",
  col= "lightblue"
  )
abline( v = median( trust.model$origCompetence ), col = "blue", lwd = 3, lty = 2 )
abline( v = mean( trust.model$origCompetence ), col = "blue", lwd = 3, lty = 2 )

# Check correlation of competencies w/ other variables.
library( Hmisc )
rcorr( 
  cbind(
    apply( trust.dat, 1, mean ), trust.model$origCompetence, apply( rh.dat, 1, mean ),apply( ps.dat, 1, mean ) 
    ) 
  )

plot( trust.model$origCompetence, apply( rh.dat, 1, mean ) )
abline( lm( apply( rh.dat, 1, mean ) ~ trust.model$origCompetence ) )
plot( trust.model$origCompetence, apply( ps.dat, 1, mean ) )
abline( lm( apply( ps.dat, 1, mean ) ~ trust.model$origCompetence ) )

### people who have more cultural knowledge about trust:
  ### are more likely to perceive higher community relational health;
  ### are more likely to perceive higher psychological safety;
    ### embeddedness in a cultural model increases these dimensions.

dat.out <- cbind( dat$id, trust.model$origCompetence )
write.csv( dat.out, "/Users/jyoung20/Dropbox (ASU)/Perryville PAR/Projects/PAR_Trust/comps.csv" )


# ================================================================== #
# Correlational Class Analysis.

library( corclass )

# Run the corclass model.
trust.cca <- cca( trust.dat, filter.value = 0.01 )

# Modularity.
library( network )
t.mat <- trust.cca$cormat[1:dim( trust.cca$cormat )[1], 1:dim( trust.cca$cormat )[1]]
diag( t.mat ) <- 0
t.net <- as.network( t.mat, directed = FALSE )
t.net %v% "member" <- trust.cca$membership

library( intergraph )
library( igraph )
i.t.net <- asIgraph( t.net )
modularity( i.t.net, trust.cca$membership )

library( sna )
library( network )
gplot( t.net, 
       gmode = "graph",
       vertex.col = t.net %v% "member", 
       edge.lwd = t.mat, displayisolates = FALSE
       )


# ================================================================== #
# CCT pack.

library( CCTpack )

trust.dat2 <- trust.dat[which( apply( trust.dat, 1, function(x) length( unique( x ) ) ) != 1 ),]  
rh.dat2 <- rh.dat[which( apply( rh.dat, 1, function(x) length( unique( x ) ) ) != 1 ),]  

cctscree( trust.dat2, polych = FALSE ) 
cctscree( rh.dat2, polych = FALSE ) 

# Estimate the models.
trust.group1 <- cctapply(data = trust.dat2, clusters = 1, itemdiff = FALSE, samples = 10000, 
                   chains = 3, burnin = 2000, seed = 1, runchecks = FALSE)
# ...Calculating DIC
# DIC : 2987.87   pD : 371.63

trust.group2 <- cctapply(data = trust.dat2, clusters = 2, itemdiff = TRUE, samples = 10000, 
                         chains = 3, burnin = 2000, seed = 1, runchecks = FALSE)
# ...Calculating DIC
# DIC : 2281.51   pD : 475.73


!!!This is somethign I wanted to check. 

Looking at whether there are groups that agree on these items.

rh.group1 <- cctapply(data = rh.dat2, clusters = 1, itemdiff = TRUE, samples = 10000, 
                      chains = 3, burnin = 2000, seed = 1, runchecks = FALSE)


rh.group2 <- cctapply(data = rh.dat2, clusters = 2, itemdiff = TRUE, samples = 10000, 
                      chains = 3, burnin = 2000, seed = 1, runchecks = FALSE)


rh.group3 <- cctapply(data = rh.dat2, clusters = 3, itemdiff = TRUE, samples = 10000, 
                         chains = 3, burnin = 2000, seed = 1, runchecks = FALSE)


# Check the results.
cctresults( trust.group1 ) 	
cctresults( trust.group2 ) 	


setwd( "/Users/jyoung20/Dropbox (ASU)/Perryville PAR/" )
save.image("temp.trash.R")
setwd( "/Users/jyoung20/Dropbox (ASU)/Perryville PAR/" )

save.image("temp.trash.R")


!!!Here with checking on this.

# Save the results. 

#!!!THIS IS NOT WHAT YOU WANT, YOU NEED TO THINK ABOUT HOW BEST TO EXPORT THIS
#!!!IT CRANKS OUT A BUNCH OF FILES
setwd( "/Users/jyoung20/Dropbox (ASU)/Perryville PAR/" )
cctexport( trust.group1, filename="trust.group1.Rdata" ) 
cctexport( trust.group2, filename="trust.group2.Rdata" ) 

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


HERE! Not working.

trust.group3 <- cctapply(data = trust.dat2, clusters = 3, itemdiff = FALSE, samples = 10000, 
                         chains = 3, burnin = 2000, seed = 999999, runchecks = FALSE)

cctsubj(trust.group2) 
cctsubjhdi(trust.group2)
cctitem( trust.group2 )

HERE: Need to figure out the stuff you want to export.

# ################################################################## #
# END OF SYNTAX FILE.
# ################################################################## #


# ================================================================== #
# PAPER TITLE
# ================================================================== #

!!!!BUILD OUT: this needs to show the model fit for all the models!!!



# This syntax file assess model fit for models estimated
# in the trust-culture-heterogeneity-analysis.R file


# ================================================================== #
# Setup ----

# Clear the workspace
rm( list = ls() )

# Load the libraries used
library( CCTpack )  # for the cultural consensus models
library( here )     # for calling local directory
library( dplyr )    # for working with the data
library( reshape2 ) # for reworking the data
library( ggplot2 )  # for plotting the agreement matrix


# ================================================================== #
# Load the estimated models ----

!!!HERE: Need to save each as an .rds
!!!need to export the fit for each one and save it as a pdf


# load the object and set the name
trust.fit.2.diff <- load( here( "trust-culture-rodeo/trust.fit.2.diff.Rdata" ) )
trust.fit.2.diff <- cctfit

# plot the posterior predictive model checks
cctppc( trust.fit.2.diff )

# export the image
pdf( file = here( "trust-culture-figures/trust.fit.2.diff.diagnostics.pdf" ) )
cctppc( trust.fit.2.diff )
dev.off()



!!!Need to figure out how to plot the pd and DIC info or how to report it
Maybe you create a table for the appendix???








!!!HERE with cleaning up

these will be the model fit appendix
these will also have the code for the .rds files to use

# ----
# Estimated model for 1 cultural model
# pD = 252.4 and DIC = 3304.4

# load the object and set the name
trust.fit.1 <- load( here( "trust-culture-rodeo/trust.fit.1.Rdata" ) )
trust.fit.1 <- cctfit

# plot the posterior predictive model checks
cctppc( trust.fit.1 )



# ----
# Estimate the model for 2 cultural models
# pD = 491.2 and DIC = 2872.8
trust.fit.2 <- cctapply( 
  data = trust.vars[,-1], 
  clusters = 2, 
  itemdiff = FALSE, 
  runchecks = FALSE,
  samples = 10000, 
  chains = 3, 
  burnin = 2000, 
  seed = 12, 24, 36
)
cctresults( trust.fit.2 )
trust.fit.2
cctppc( trust.fit.2 )
cctexport( trust.fit.2, filename = "trust.fit.2.Rdata")



# ----
# Estimate the model for 2 cultural models w/ item difficulty
# pD = 474.2 and DIC = 2493.3
trust.fit.2.diff <- cctapply( 
  data = trust.vars[,-1], 
  clusters = 2, 
  itemdiff = TRUE, 
  runchecks = FALSE,
  samples = 10000, 
  chains = 3, 
  burnin = 2000, 
  seed = 5, 7, 9
)
cctresults( trust.fit.2.diff )
trust.fit.2.diff
cctppc( trust.fit.2.diff )
cctexport( trust.fit.2.diff, filename = "trust.fit.2.diff.Rdata")



# ----
# Estimate the model for 3 cultural models
# pD = 548.1 and DIC = 2728.4
trust.fit.3 <- cctapply( 
  data = trust.vars[,-1], 
  clusters = 3, 
  itemdiff = FALSE, 
  runchecks = FALSE,
  samples = 10000, 
  chains = 3, 
  burnin = 2000, 
  seed = 12, 24, 36
)
cctresults( trust.fit.3 )
trust.fit.3
cctppc( trust.fit.3 )
cctexport( trust.fit.3, filename = "trust.fit.3.Rdata")



# ----
# Estimate the model for 4 cultural models
# pD = 480.0 and DIC = 2557.2
trust.fit.4 <- cctapply( 
  data = trust.vars[,-1], 
  clusters = 4, 
  itemdiff = FALSE, 
  runchecks = FALSE,
  samples = 10000, 
  chains = 3, 
  burnin = 2000, 
  seed = 12, 24, 36
)
cctresults( trust.fit.4 )
trust.fit.4
cctppc( trust.fit.4 )
cctexport( trust.fit.4, filename = "trust.fit.4.Rdata")



# ----
# Estimate the model for 5 cultural models
# pD = 573.6 and DIC = 2578.9
trust.fit.5 <- cctapply( 
  data = trust.vars[,-1], 
  clusters = 5, 
  itemdiff = FALSE, 
  runchecks = FALSE,
  samples = 10000, 
  chains = 3, 
  burnin = 2000, 
  seed = 12, 24, 36
)
cctresults( trust.fit.5 )
trust.fit.5
cctppc( trust.fit.5 )
cctexport( trust.fit.5, filename = "trust.fit.5.Rdata")


# ----
# Estimate the model for 6 cultural models
# pD = 601.3 and DIC = 2517.2
trust.fit.6 <- cctapply( 
  data = trust.vars[,-1], 
  clusters = 6, 
  itemdiff = FALSE, 
  runchecks = FALSE,
  samples = 10000, 
  chains = 3, 
  burnin = 2000, 
  seed = 12, 24, 36
)
cctresults( trust.fit.6 )
trust.fit.6
cctppc( trust.fit.6 )
cctexport( trust.fit.6, filename = "trust.fit.6.Rdata")







!!!!the export does not give you the posterior predictive checks
you need to write some code to call these back up and estimate them
from the data file

Need to rerun 3 and run the export function



!!!HERE WITH ESTIMATEING THESE

!!!AFter you are done here, you need to update the "trust-culture-heterogeneity-analysis.R" file

so you can load the file and then run these to export them


















###CLEAN
#
#
## Run the function to replace the missing values
## replace 67 missing values: table( is.na( trust.vars[,-1] ) )
#trust.vars2   <- NA.replace( as.matrix( trust.vars[,-1] ), round( runif( n = 1, min = 0, max = 1 ), 0 ) )
#trust.vars <- cbind( dat$id, trust.vars2 )
#
# Exclude cases with repeated responses.
# drop 72 cases that lack variation in there responses.
# then drop 13 cases with missing data.

#trust.vars2 <- trust.vars[ 
#  which( apply( trust.vars[,-1], 1, function( x ) length( unique( x ) ) ) != 1 ), ]
#trust.vars2 <- na.omit( trust.vars2 )
#trust.vars  <- trust.vars2


# ================================================================== #
# Estimate models ----





trust.fit.5 <- cctapply( 
  data = trust.vars[,-1], 
  clusters = 5, 
  itemdiff = FALSE, 
  runchecks = FALSE,
  samples = 10000, 
  chains = 3, 
  burnin = 2000, 
  seed = 12, 24, 36
)
cctresults( trust.fit.5 )
trust.fit.5
cctppc( trust.fit.5 )
# pD = 573.6 and DIC = 2578.9


# merge the competencies to the ids
trust.comp <- cbind( trust.vars[,1], trust.fit.2$subj[,3], trust.fit.2$subj[,2] )
colnames( trust.comp ) <- c( "id", "comp", "group" )
trust.comp <- as.data.frame( trust.comp )

# recode group 2 to be the 1 and the 1 to be a 0
trust.comp$group[trust.comp$group == 1] <- 0
trust.comp$group[trust.comp$group == 2] <- 1

# write the file out
write.csv( trust.comp, "PAR-Trust-CCT-estimates.csv" )












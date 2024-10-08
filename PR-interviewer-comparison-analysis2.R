# ================================================================== #
# PAPER TITLE
# ================================================================== #

# DOCUMENT!!!


# ================================================================== #
# Setup ----

# Clear the workspace
rm( list = ls() )

# Load the libraries
library( dplyr ) # for working with the data
library( here )  # to call local directory

 
# ================================================================== #
# load the data
dat <- readRDS( file = here( "PR-interviewer-comparison-rodeo/trust.rhps.cntrls.vars.data.rds" ) )

# coerce to a data frame
dat <- as.data.frame( dat )


# ================================================================== #
# Create the function to build the set of t.tests

t.test.data <- function( dat.to.use, vars.to.use, group.var, alpha = 0.05 ){
  
  # Create the objects to assign values
  tests <- as.list( NULL )
  values <- NA
  pval <- NA
  uci <- NA
  lci <- NA
  
  # Perform the tests
  for( i in 1: ncol( vars.to.use ) ){
    tests[[i]] <-
      t.test( vars.to.use[[i]] ~ group.var, data = dat.to.use )
  }
  
  # Extract the values
  for( i in 1: length( tests ) ){
    values[i] <- round( ( tests[[i]]$estimate[1] - tests[[i]]$estimate[2] ), 3 )
    pval[i]   <- round( tests[[i]]$p.value, 3 )
    lci[i]    <- round( tests[[i]]$conf.int[1], 3 )
    uci[i]    <- round( tests[[i]]$conf.int[2], 3 )
  }
  
  # Create the bonferroni corrected pvalues
  bonf.pval <- round( alpha / length( tests ), 3 )
  
  # Create reject/fail to reject
  reject <- NULL
  reject <- pval < bonf.pval
  
  # Build the data object
  results <- cbind( values, pval, bonf.pval, reject, lci, uci )
  colnames( results ) <- c( "tvalue", "pvalue", "Bonf.Pvalue", "Reject?", "LCI", "UCI" )
  rownames( results ) <- names( vars.to.use )
  
  return( as.data.frame( results ) )
}


# ================================================================== #
# Create the function to plot the results

plot.ttests <- function( t.results, the.title ){
  
  point    <- t.results$tvalue
  upper.ci <- t.results$UCI
  lower.ci <- t.results$LCI
  
  x.ax <- seq( 1, dim( t.results )[1], length.out = dim( t.results )[1] )
  y.ax <- seq( 
    min( c( upper.ci,lower.ci ) ), 
    max( c( upper.ci,lower.ci ) ), 
    length.out= dim( t.results )[1] 
    )
  
  plot(x.ax,
       y.ax,
       type = "n",
       ylab = "t-test coefficient w/ 95% CI",
       xlab = "",
       xaxt = "n"
  )
  
  points( x.ax, point )
  segments(x.ax, upper.ci, x.ax, lower.ci)
  abline( h = 0, lty = 2 )
  axis( side = 1, at = x.ax, las = 3, labels = custom_labels )
  title( the.title )
}


# ================================================================== #
# Compare trust covariates by interviewer.

# Get the data you want to compare
cov.dat <- dat %>% 
  select(
    id, Interviewer, 
    trust1:trust15  )

# Create the variables to perform the tests on
cov.vars.to.use <- cov.dat %>% 
  select( -c( id, Interviewer ) )

# Create vector of names for the plot
custom_labels <- c(
  paste("T ",rep( 1:15 ), sep = "" )
)


# Create the object of t-test results
cov.t.results <- t.test.data(
  cov.dat, cov.vars.to.use, cov.dat$Interviewer 
)

# plot the tests
plot.ttests( cov.t.results, ""  )

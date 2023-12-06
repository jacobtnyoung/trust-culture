# ################################################################## #
# Perryville PAR Trust.
# ################################################################## #

# This syntax file analyzes the trust items from section 3 of the instrument.
# This file sources a build file.

# ================================================================== #
# Setup ----

# Clear the workspace ----
rm( list = ls() )

# Load the libraries ----
library( sandwich )
library( lmtest )
library( modelsummary )
library( ggplot2 )
library( here )
library( dplyr )

# Pull in the object to examine ----
import::here( "dat.for.models",
              .from = here::here("PV-PAR-Trust-BUILD-syntax.R"),
              .character_only = TRUE )


# ================================================================== #
# Estimate models ----

rh.m.1.robust <- coeftest( 
  lm( relhlth ~ trust.competency , data = dat.for.models  ), vcov = vcovHC, type = "HC1" )

rh.m.2.robust <- coeftest( 
  lm( relhlth ~ trust.competency + safety, data = dat.for.models  ), vcov = vcovHC, type = "HC1" )

ps.m.1.robust <- coeftest( 
  lm( safety ~ trust.competency , data = dat.for.models  ), vcov = vcovHC, type = "HC1" )

ps.m.2.robust <- coeftest( 
  lm( safety ~ trust.competency + relhlth, data = dat.for.models  ), vcov = vcovHC, type = "HC1" )

rh.m.3.robust <- coeftest( 
  lm( 
    relhlth ~ trust.competency 
    + safety + interviewer + randomized 
    + age + timein_yrs + black + hispanic, 
    data = dat.for.models  ), vcov = vcovHC, type = "HC1" )

ps.m.3.robust <- coeftest( 
  lm( 
    safety ~ trust.competency 
    + relhlth + interviewer + randomized 
    + age + timein_yrs + black + hispanic, 
    data = dat.for.models  ), vcov = vcovHC, type = "HC1" )


# ================================================================== #
# Plot model results ----

# Create the modeling object
rh.models <- list( 
  "Model 1: Cultural Embededdness (bivariate)" = rh.m.1.robust, 
  "Model 2: Including Psychological Safety" = rh.m.2.robust,  
  "Model 3: Full" = rh.m.3.robust )
ps.models <- list( 
  "Cultural Embededdness (bivariate)" = ps.m.1.robust, 
  "Model 2: Including Relational Health" = ps.m.2.robust, 
  "Model 3: Full" = ps.m.3.robust )

# Reorder coefficients
rh.coef.names <- c( 
  "randomized" = "Randomized",
  "interviewer" = "Incarcerated Interviewer",
  "hispanic" = "Hispanic",
  "black" = "African American/Black",
  "timein_yrs" = "Years in Prison",
  "age" = "Age (years)",
  "safety" = "Psychological Safety",
  "trust.competency" = "Cultural Embeddedness"
)

ps.coef.names <- c( 
  "randomized" = "Randomized",
  "interviewer" = "Incarcerated Interviewer",
  "hispanic" = "Hispanic",
  "black" = "African American/Black",
  "timein_yrs" = "Years in Prison",
  "age" = "Age (years)",
  "relhlth" = "Relational Health",
  "trust.competency" = "Cultural Embeddedness"
)

# Draw plots
rh.plot <- 
  modelplot( 
    rh.models,
    coef_map = rh.coef.names,
    background = list( geom_vline( xintercept = 0, color = "lightblue" ) ),
    coef_omit = "Interc" ) +
  labs( x = "Coefficents",
        y = "Variables",
        title = "Models Predicting Relational Health",
        caption = "" ) +
  scale_color_brewer( type = "qual" )
rh.plot

ps.plot <- 
  modelplot( 
    ps.models,
    coef_map = ps.coef.names,
    background = list( geom_vline( xintercept = 0, color = "lightblue" ) ),
    coef_omit = "Interc" ) +
  labs( x = "Coefficents",
        y = "Variables",
        title = "Models Predicting Psychological Safety",
        caption = "" ) +
  scale_color_brewer( type = "qual" )
ps.plot


# ================================================================== #
# CCTpack models ----

dat <- read.csv( here( "PAR-Trust-data-from-CCT/PAR-Trust-CCT-estimates.csv" ) )
comp.dat <- left_join( dat.for.models, dat )
comp.dat2 <- na.omit( comp.dat )

intX <- 
  ( comp.dat2$group - mean( comp.dat2$group ) ) *
  ( comp.dat2$comp - mean( comp.dat2$comp ) )


rh.1.robust <- coeftest( 
  lm( 
    relhlth ~ comp + group + intX, 
    data = comp.dat2  ), vcov = vcovHC, type = "HC1" )
rh.2.robust <- coeftest( 
  lm( 
    relhlth ~ comp + group + intX 
    + safety, 
    data = comp.dat2  ), vcov = vcovHC, type = "HC1" )
rh.3.robust <- coeftest( 
  lm( 
    relhlth ~ comp + group + intX 
    + safety + interviewer + randomized 
    + age + timein_yrs + black + hispanic, 
    data = comp.dat2  ), vcov = vcovHC, type = "HC1" )

ps.1.robust <- coeftest( 
  lm( 
    safety ~ comp + group + intX, 
    data = comp.dat2  ), vcov = vcovHC, type = "HC1" )
ps.2.robust <- coeftest( 
  lm( 
    safety ~ comp + group + intX 
    + relhlth, 
    data = comp.dat2  ), vcov = vcovHC, type = "HC1" )
ps.3.robust <- coeftest( 
  lm( 
    safety ~ comp + group + intX 
    + relhlth + interviewer + randomized 
    + age + timein_yrs + black + hispanic, 
    data = comp.dat2  ), vcov = vcovHC, type = "HC1" )

rh.models <- list( 
  "Model 1: ???" = rh.1.robust, 
  "Model 2: Including Psychological Safety" = rh.2.robust,  
  "Model 3: Full" = rh.3.robust )
ps.models <- list( 
  "Model 1: ???" = ps.1.robust, 
  "Model 2: Including Relational Health" = ps.2.robust, 
  "Model 3: Full" = ps.3.robust )

# Reorder coefficients
rh.coef.names <- c( 
  "randomized" = "Randomized",
  "interviewer" = "Incarcerated Interviewer",
  "hispanic" = "Hispanic",
  "black" = "African American/Black",
  "timein_yrs" = "Years in Prison",
  "age" = "Age (years)",
  "safety" = "Psychological Safety",
  "comp" = "Competency",
  "group" = "Group",
  "intX" = "Interaction"
)

ps.coef.names <- c( 
  "randomized" = "Randomized",
  "interviewer" = "Incarcerated Interviewer",
  "hispanic" = "Hispanic",
  "black" = "African American/Black",
  "timein_yrs" = "Years in Prison",
  "age" = "Age (years)",
  "relhlth" = "Relational Health",
  "comp" = "Competency",
  "group" = "Group",
  "intX" = "Interaction"
)

# Draw plots
rh.plot <- 
  modelplot( 
    rh.models,
    coef_map = rh.coef.names,
    background = list( geom_vline( xintercept = 0, color = "lightblue" ) ),
    coef_omit = "Interc" ) +
  labs( x = "Coefficents",
        y = "Variables",
        title = "Models Predicting Relational Health",
        caption = "" ) +
  scale_color_brewer( type = "qual" )
rh.plot

ps.plot <- 
  modelplot( 
    ps.models,
    coef_map = ps.coef.names,
    background = list( geom_vline( xintercept = 0, color = "lightblue" ) ),
    coef_omit = "Interc" ) +
  labs( x = "Coefficents",
        y = "Variables",
        title = "Models Predicting Psychological Safety",
        caption = "" ) +
  scale_color_brewer( type = "qual" )
ps.plot


# ################################################################## #
# END OF SYNTAX FILE.
# ################################################################## #

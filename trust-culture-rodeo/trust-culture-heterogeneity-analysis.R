# ================================================================== #
# PAPER TITLE
# ================================================================== #

# This syntax file analyses the results from the 
# trust-culture-heterogeneity-estimation.R file


# ================================================================== #
# Setup ----

# Clear the workspace
rm( list = ls() )

# Load the libraries used
library( CCTpack )  # for the cultural consensus models
library( here )     # for calling local directory
library( dplyr )    # for working with the data
library( reshape2 ) # for reworking the data
library( ggplot2 )  # for plotting


# ================================================================== #
# load the trust variables ----

# load the data
trust.vars <- readRDS( file = here( "trust-culture-rodeo/trust.fit.data.rds" ) )


# ================================================================== #
# Load the estimated models ----

# load the object and set the name
trust.fit.2.diff <- load( here( "trust-culture-rodeo/trust.fit.2.diff.Rdata" ) )
trust.fit.2.diff <- cctfit


# ================================================================== #
# Build Figure # of cultural knowledge ----

# build the object to plot
knowledgeDat <- data.frame(
  assignmentID = trust.fit.2.diff$subj[,1],
  culture      = trust.fit.2.diff$subj[,2],
  knowledge    = trust.fit.2.diff$subj[,3]
)

# Calculate mean for each group
culturalMeansSds <- knowledgeDat %>%
  group_by( culture ) %>%
  summarise( mean_knowledge = round( mean( knowledge ), 2 ),
             sd_knowledge = round( sd( knowledge ), 2 ) )

# Add a new variable that ranks responses within each group
knowledgeDat <- knowledgeDat %>%
  arrange( culture, knowledge ) %>%
  mutate( rank_knowledge = row_number() )

# Create the scatterplot
knowledgePlot <- ggplot( knowledgeDat,
        aes( x = rank_knowledge, 
             y = knowledge, 
             color = as.factor( culture ) ) ) +
  geom_point() + 
  labs( title = "Scatterplot of Responses by Group",
        x = "Respondents",
        y = "Knowledge/Expertise") +
  geom_hline( data = culturalMeansSds, 
              aes( yintercept = mean_knowledge, colour = as.factor( culture ) ) ) +
    theme_minimal() +
  theme( legend.position = "top",
         axis.text.x = element_blank() ) +
  guides( color = guide_legend( title = "Group" ) )
print( knowledgePlot )

# export the image
pdf( file = here( "trust-culture-figures/trust.knowledge.pdf" ) )
print( knowledgePlot )
dev.off()


# ================================================================== #
# Cultural model assignment and knowledge as a predictor ----

# get the relational health and psychological safety variables
rhpsDat <- readRDS( file = here( "trust-culture-rodeo/trust.rhps.cntrls.vars.data.rds" ) )

# merge the cultural model data with the relational health and psychological safety data

match.id <- data.frame(
  ID = trust.vars[,1]
)

modelDat <- inner_join( match.id, rhpsDat )

knowledgeDat <- knowledgeDat %>% 
  arrange( assignmentID )

modelDat <- left_join( knowledgeDat, modelDat )


# ================================================================== #
# Build a table of ttests and correlations ----

statsTab <- matrix( NA, nrow = 9, ncol = 2 )

statsTab[1,1] <- t.test( rh ~ culture, data = modelDat )$statistic
statsTab[2,1] <- cor.test( formula = ~ rh + knowledge, data = modelDat )$estimate
statsTab[3,1] <- t.test( rh ~ interviewer, data = modelDat )$statistic
statsTab[4,1] <- t.test( rh ~ randomized, data = modelDat )$statistic
statsTab[5,1] <- cor.test( formula = ~ rh + age, data = modelDat )$estimate
statsTab[6,1] <- t.test( rh ~ white, data = modelDat )$statistic
statsTab[7,1] <- t.test( rh ~ black, data = modelDat )$statistic
statsTab[8,1] <- t.test( rh ~ hispanic, data = modelDat )$statistic
statsTab[9,1] <- cor.test( formula = ~ rh + timein_yrs, data = modelDat )$estimate

statsTab[1,2] <- t.test( rh ~ culture, data = modelDat )$p.value
statsTab[2,2] <- cor.test( formula = ~ rh + knowledge, data = modelDat )$p.value
statsTab[3,2] <- t.test( rh ~ interviewer, data = modelDat )$p.value
statsTab[4,2] <- t.test( rh ~ randomized, data = modelDat )$p.value
statsTab[5,2] <- cor.test( formula = ~ rh + age, data = modelDat )$p.value
statsTab[6,2] <- t.test( rh ~ white, data = modelDat )$p.value
statsTab[7,2] <- t.test( rh ~ black, data = modelDat )$p.value
statsTab[8,2] <- t.test( rh ~ hispanic, data = modelDat )$p.value
statsTab[9,2] <- cor.test( formula = ~ rh + timein_yrs, data = modelDat )$p.value

colnames( statsTab ) <- c( "statistic", "pvalue" )
rownames( statsTab ) <- c( "Cultural Model", "Knowledge", "Interviewer", "Randomized", "Age", "White", "Black", "Hispanic", "Time In" )
statsTab <- round( statsTab, 2 )
statsTab


then need the one for psychological safety





model1dat <- modelDat %>% 
  filter( culture == 1 )

cultTab <- matrix( NA, nrow = 9, ncol = 2 )

cultTab[1,1] <- cor.test( formula = ~ rh + knowledge, data = model1dat )$estimate
cultTab[2,1] <- cor.test( formula = ~ ps + knowledge, data = model1dat )$estimate
cultTab[3,1] <- t.test( knowledge ~ interviewer, data = model1dat )$statistic
cultTab[4,1] <- t.test( knowledge ~ randomized, data = model1dat )$statistic
cultTab[5,1] <- cor.test( formula = ~ knowledge + age, data = model1dat )$estimate
cultTab[6,1] <- t.test( knowledge ~ white, data = model1dat )$statistic
cultTab[7,1] <- t.test( knowledge ~ black, data = model1dat )$statistic
cultTab[8,1] <- t.test( knowledge ~ hispanic, data = model1dat )$statistic
cultTab[9,1] <- cor.test( formula = ~ knowledge + timein_yrs, data = model1dat )$estimate

cultTab[1,2] <- cor.test( formula = ~ rh + knowledge, data = model1dat )$p.value
cultTab[2,2] <- cor.test( formula = ~ ps + knowledge, data = model1dat )$p.value
cultTab[3,2] <- t.test( knowledge ~ interviewer, data = model1dat )$p.value
cultTab[4,2] <- t.test( knowledge ~ randomized, data = model1dat )$p.value
cultTab[5,2] <- cor.test( formula = ~ knowledge + age, data = model1dat )$p.value
cultTab[6,2] <- t.test( knowledge ~ white, data = model1dat )$p.value
cultTab[7,2] <- t.test( knowledge ~ black, data = model1dat )$p.value
cultTab[8,2] <- t.test( knowledge ~ hispanic, data = model1dat )$p.value
cultTab[9,2] <- cor.test( formula = ~ knowledge + timein_yrs, data = model1dat )$p.value



model1dat <- modelDat %>% 
  filter( culture == 2 )

cultTab <- matrix( NA, nrow = 9, ncol = 2 )

cultTab[1,1] <- cor.test( formula = ~ rh + knowledge, data = model1dat )$estimate
cultTab[2,1] <- cor.test( formula = ~ ps + knowledge, data = model1dat )$estimate
cultTab[3,1] <- t.test( knowledge ~ interviewer, data = model1dat )$statistic
cultTab[4,1] <- t.test( knowledge ~ randomized, data = model1dat )$statistic
cultTab[5,1] <- cor.test( formula = ~ knowledge + age, data = model1dat )$estimate
cultTab[6,1] <- t.test( knowledge ~ white, data = model1dat )$statistic
cultTab[7,1] <- t.test( knowledge ~ black, data = model1dat )$statistic
cultTab[8,1] <- t.test( knowledge ~ hispanic, data = model1dat )$statistic
cultTab[9,1] <- cor.test( formula = ~ knowledge + timein_yrs, data = model1dat )$estimate

cultTab[1,2] <- cor.test( formula = ~ rh + knowledge, data = model1dat )$p.value
cultTab[2,2] <- cor.test( formula = ~ ps + knowledge, data = model1dat )$p.value
cultTab[3,2] <- t.test( knowledge ~ interviewer, data = model1dat )$p.value
cultTab[4,2] <- t.test( knowledge ~ randomized, data = model1dat )$p.value
cultTab[5,2] <- cor.test( formula = ~ knowledge + age, data = model1dat )$p.value
cultTab[6,2] <- t.test( knowledge ~ white, data = model1dat )$p.value
cultTab[7,2] <- t.test( knowledge ~ black, data = model1dat )$p.value
cultTab[8,2] <- t.test( knowledge ~ hispanic, data = model1dat )$p.value
cultTab[9,2] <- cor.test( formula = ~ knowledge + timein_yrs, data = model1dat )$p.value














!!!need to include the controls
That controls file needs to not be on the repo

# Create the objects to join ----
dat.for.models <- data.frame(
  id = dat$id,
  trust.competency = trust.model$origCompetence,
  relhlth = apply( rh.dat, 1, mean ),
  safety =  apply( ps.dat, 1, mean ),
  interviewer = dat$InterviewerType,
  randomized = dat$Randomize,
  age = dat$S4Q1,
  white = dat$white,  
  black = dat$black,
  hispanic = dat$hispanic,
  timein_yrs = dat$timein_yrs
)














# ================================================================== #
# Build the objects for proportions ----

# !!!This needs to be cleaned up

# create table with the proportions
a <- round( apply( trust.vars[,-1], 2, mean ), 2 )
b <- trust.fit.2.diff$item[,2]
c <- trust.fit.2.diff$item[,3]
d <- trust.fit.2.diff$item[,4]
e <- trust.fit.2.diff$item[,5]

# print the table
tab <- cbind( a, b, c, d, e )

# report the means and standard deviation
round( apply( tab, 2, mean ), 2 ) 
round( apply( tab, 2, sd ), 2 ) 


# examine the membership
round( table( trust.fit.2.diff$respmem ) / length( trust.fit.2.diff$respmem ), 2 ) 



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
# Build Figure 4 of cultural knowledge ----

# build the object to plot
knowledgeDat <- data.frame(
  assignmentID = trust.fit.2.diff$subj[,1],
  culture      = trust.fit.2.diff$subj[,2],
  knowledge    = trust.fit.2.diff$subj[,3]
)

# recode the cultural model assignment to make group 2 group 1
# and group 1 group 2 (this is for ease of interpretation)

cultureRecoded <- knowledgeDat$culture   
cultureRecoded[ knowledgeDat$culture == 2 ] <- 0
cultureRecoded[ knowledgeDat$culture == 1 ] <- 2
cultureRecoded[ knowledgeDat$culture == 2 ] <- 1
knowledgeDat$cultureRecoded <- cultureRecoded

# Calculate mean for each group
culturalMeansSds <- knowledgeDat %>%
  group_by( cultureRecoded ) %>%
  summarise( mean_knowledge = round( mean( knowledge ), 2 ),
             sd_knowledge = round( sd( knowledge ), 2 ) )

# Add a new variable that ranks responses within each group
knowledgeDat <- knowledgeDat %>%
  arrange( cultureRecoded, knowledge ) %>%
  mutate( rank_knowledge = row_number() )

# Create the scatterplot
knowledgePlot <- ggplot( knowledgeDat,
        aes( x = rank_knowledge, 
             y = knowledge, 
             color = as.factor( cultureRecoded ) ) ) +
  geom_point() + 
  labs( title = "Scatterplot of Respondent Knowledge by Relationship Model",
        x = "Respondents",
        y = "Knowledge") +
  geom_hline( data = culturalMeansSds, 
              aes( yintercept = mean_knowledge, colour = as.factor( cultureRecoded ) ) ) +
    theme_minimal() +
  theme( legend.position = "top",
         axis.text.x = element_blank() ) +
  guides( color = guide_legend( title = "Relationship Model: " ) )
print( knowledgePlot )

# export the image
pdf( file = here( "trust-culture-figures/figure-4-trust-knowledge.pdf" ) )
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

# Relational health

rhTab <- matrix( NA, nrow = 10, ncol = 2 )

rhTab[1,1] <- t.test( rh ~ cultureRecoded, data = modelDat )$statistic
rhTab[2,1] <- cor.test( formula = ~ rh + knowledge, data = modelDat )$estimate
rhTab[3,1] <- cor.test( formula = ~ rh + ps, data = modelDat )$estimate
rhTab[4,1] <- cor.test( formula = ~ rh + age, data = modelDat )$estimate
rhTab[5,1] <- t.test( rh ~ white, data = modelDat )$statistic
rhTab[6,1] <- t.test( rh ~ black, data = modelDat )$statistic
rhTab[7,1] <- t.test( rh ~ hispanic, data = modelDat )$statistic
rhTab[8,1] <- cor.test( formula = ~ rh + timein_yrs, data = modelDat )$estimate
rhTab[9,1] <- t.test( rh ~ interviewer, data = modelDat )$statistic
rhTab[10,1] <- t.test( rh ~ randomized, data = modelDat )$statistic

rhTab[1,2] <- t.test( rh ~ cultureRecoded, data = modelDat )$p.value
rhTab[2,2] <- cor.test( formula = ~ rh + knowledge, data = modelDat )$p.value
rhTab[3,2] <- cor.test( formula = ~ rh + ps, data = modelDat )$p.value
rhTab[4,2] <- cor.test( formula = ~ rh + age, data = modelDat )$p.value
rhTab[5,2] <- t.test( rh ~ white, data = modelDat )$p.value
rhTab[6,2] <- t.test( rh ~ black, data = modelDat )$p.value
rhTab[7,2] <- t.test( rh ~ hispanic, data = modelDat )$p.value
rhTab[8,2] <- cor.test( formula = ~ rh + timein_yrs, data = modelDat )$p.value
rhTab[9,2] <- t.test( rh ~ interviewer, data = modelDat )$p.value
rhTab[10,2] <- t.test( rh ~ randomized, data = modelDat )$p.value

colnames( rhTab ) <- c( "statistic", "pvalue" )
rownames( rhTab ) <- c( "Relationship Model", "Knowledge", "Psychological Safety", "Age", "White", "Black", "Hispanic", "Time In", "Interviewer", "Randomized" )
rhTab <- round( rhTab, 2 )
rhTab


# Psychological safety

psTab <- matrix( NA, nrow = 10, ncol = 2 )

psTab[1,1] <- t.test( ps ~ cultureRecoded, data = modelDat )$statistic
psTab[2,1] <- cor.test( formula = ~ ps + knowledge, data = modelDat )$estimate
psTab[3,1] <- cor.test( formula = ~ ps + rh, data = modelDat )$estimate
psTab[4,1] <- cor.test( formula = ~ ps + age, data = modelDat )$estimate
psTab[5,1] <- t.test( ps ~ white, data = modelDat )$statistic
psTab[6,1] <- t.test( ps ~ black, data = modelDat )$statistic
psTab[7,1] <- t.test( ps ~ hispanic, data = modelDat )$statistic
psTab[8,1] <- cor.test( formula = ~ ps + timein_yrs, data = modelDat )$estimate
psTab[9,1] <- t.test( ps ~ interviewer, data = modelDat )$statistic
psTab[10,1] <- t.test( ps ~ randomized, data = modelDat )$statistic

psTab[1,2] <- t.test( ps ~ cultureRecoded, data = modelDat )$p.value
psTab[2,2] <- cor.test( formula = ~ ps + knowledge, data = modelDat )$p.value
psTab[3,2] <- cor.test( formula = ~ ps + rh, data = modelDat )$p.value
psTab[4,2] <- cor.test( formula = ~ ps + age, data = modelDat )$p.value
psTab[5,2] <- t.test( ps ~ white, data = modelDat )$p.value
psTab[6,2] <- t.test( ps ~ black, data = modelDat )$p.value
psTab[7,2] <- t.test( ps ~ hispanic, data = modelDat )$p.value
psTab[8,2] <- cor.test( formula = ~ ps + timein_yrs, data = modelDat )$p.value
psTab[9,2] <- t.test( ps ~ interviewer, data = modelDat )$p.value
psTab[10,2] <- t.test( ps ~ randomized, data = modelDat )$p.value

colnames( psTab ) <- c( "statistic", "pvalue" )
rownames( psTab ) <- c( "Relationship Model", "Knowledge", "Relational Health", "Age", "White", "Black", "Hispanic", "Time In", "Interviewer", "Randomized" )
psTab <- round( psTab, 2 )
psTab


# Correlates with knowledge by relationship model

model1dat <- modelDat %>% 
  filter( cultureRecoded == 1 )

cM1Tab <- matrix( NA, nrow = 9, ncol = 2 )

cM1Tab[1,1] <- cor.test( formula = ~ rh + knowledge, data = model1dat )$estimate
cM1Tab[2,1] <- cor.test( formula = ~ ps + knowledge, data = model1dat )$estimate
cM1Tab[3,1] <- cor.test( formula = ~ knowledge + age, data = model1dat )$estimate
cM1Tab[4,1] <- t.test( knowledge ~ white, data = model1dat )$statistic
cM1Tab[5,1] <- t.test( knowledge ~ black, data = model1dat )$statistic
cM1Tab[6,1] <- t.test( knowledge ~ hispanic, data = model1dat )$statistic
cM1Tab[7,1] <- cor.test( formula = ~ knowledge + timein_yrs, data = model1dat )$estimate
cM1Tab[8,1] <- t.test( knowledge ~ interviewer, data = model1dat )$statistic
cM1Tab[9,1] <- t.test( knowledge ~ randomized, data = model1dat )$statistic

cM1Tab[1,2] <- cor.test( formula = ~ rh + knowledge, data = model1dat )$p.value
cM1Tab[2,2] <- cor.test( formula = ~ ps + knowledge, data = model1dat )$p.value
cM1Tab[3,2] <- cor.test( formula = ~ knowledge + age, data = model1dat )$p.value
cM1Tab[4,2] <- t.test( knowledge ~ white, data = model1dat )$p.value
cM1Tab[5,2] <- t.test( knowledge ~ black, data = model1dat )$p.value
cM1Tab[6,2] <- t.test( knowledge ~ hispanic, data = model1dat )$p.value
cM1Tab[7,2] <- cor.test( formula = ~ knowledge + timein_yrs, data = model1dat )$p.value
cM1Tab[8,2] <- t.test( knowledge ~ interviewer, data = model1dat )$p.value
cM1Tab[9,2] <- t.test( knowledge ~ randomized, data = model1dat )$p.value

colnames( cM1Tab ) <- c( "statistic", "pvalue" )
rownames( cM1Tab ) <- c( "Relational Health", "Psychological Safety", "Age", "White", "Black", "Hispanic", "Time In", "Interviewer", "Randomized" )
cM1Tab <- round( cM1Tab, 2 )
cM1Tab


model2dat <- modelDat %>% 
  filter( cultureRecoded == 2 )

cM2Tab <- matrix( NA, nrow = 9, ncol = 2 )

cM2Tab[1,1] <- cor.test( formula = ~ rh + knowledge, data = model2dat )$estimate
cM2Tab[2,1] <- cor.test( formula = ~ ps + knowledge, data = model2dat )$estimate
cM2Tab[3,1] <- cor.test( formula = ~ knowledge + age, data = model2dat )$estimate
cM2Tab[4,1] <- t.test( knowledge ~ white, data = model2dat )$statistic
cM2Tab[5,1] <- t.test( knowledge ~ black, data = model2dat )$statistic
cM2Tab[6,1] <- t.test( knowledge ~ hispanic, data = model2dat )$statistic
cM2Tab[7,1] <- cor.test( formula = ~ knowledge + timein_yrs, data = model2dat )$estimate
cM2Tab[8,1] <- t.test( knowledge ~ interviewer, data = model2dat )$statistic
cM2Tab[9,1] <- t.test( knowledge ~ randomized, data = model2dat )$statistic

cM2Tab[1,2] <- cor.test( formula = ~ rh + knowledge, data = model2dat )$p.value
cM2Tab[2,2] <- cor.test( formula = ~ ps + knowledge, data = model2dat )$p.value
cM2Tab[3,2] <- cor.test( formula = ~ knowledge + age, data = model2dat )$p.value
cM2Tab[4,2] <- t.test( knowledge ~ white, data = model2dat )$p.value
cM2Tab[5,2] <- t.test( knowledge ~ black, data = model2dat )$p.value
cM2Tab[6,2] <- t.test( knowledge ~ hispanic, data = model2dat )$p.value
cM2Tab[7,2] <- cor.test( formula = ~ knowledge + timein_yrs, data = model2dat )$p.value
cM2Tab[8,2] <- t.test( knowledge ~ interviewer, data = model2dat )$p.value
cM2Tab[9,2] <- t.test( knowledge ~ randomized, data = model2dat )$p.value

colnames( cM2Tab ) <- c( "statistic", "pvalue" )
rownames( cM2Tab ) <- c( "Relational Health", "Psychological Safety", "Age", "White", "Black", "Hispanic", "Time In", "Interviewer", "Randomized" )
cM2Tab <- round( cM2Tab, 2 )
cM2Tab


# ================================================================== #
# Build the objects for proportions ----

# create table with the proportions
trust <- round( apply( trust.vars[,-1], 2, mean ), 2 )
m1ans <- trust.fit.2.diff$item[,3]
m2ans <- trust.fit.2.diff$item[,2]
m1diff <- trust.fit.2.diff$item[,5]
m2diff <- trust.fit.2.diff$item[,4]

# print the table
tab <- cbind( trust, m1ans, m2ans, m1diff, m2diff )

# report the means and standard deviation
round( apply( tab, 2, mean ), 2 ) 
round( apply( tab, 2, sd ), 2 ) 

# examine the membership
round( table( trust.fit.2.diff$respmem ) / length( trust.fit.2.diff$respmem ), 2 ) 


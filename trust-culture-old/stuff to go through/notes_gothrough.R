



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



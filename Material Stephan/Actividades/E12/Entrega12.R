
library( corrplot )
library( tidyr )
library( car )
library( caret )
library( pROC )

# Using .csv file with Phishing Websites Features. If this file doesn't exists 
# in the repository indicating below, is necessary to change dir path.
dir <- "~/R/codes/Entrega12/"
basename <- "Phishing.csv"
file <- file.path( dir, basename )
population <- read.csv( file = file )

# Setting seed using date of birth and a sample size of 500.
set.seed( 199708 )
sample.size <- 500
population.size <- nrow( population )

# Use correlation matrix to select preditors
correlation <- round( cor( population ), 1 )
corrplot( correlation, method = "number", type = "upper" )
# The correlation matrix shows us that one of the best predictor is 
# "Page_Rank" and doesn't have collinearity problems (test and error), 
# thus, we will continue to work with this variable in the next steps.

# Set data
population$Result <- factor( population$Result )
population$Prefix_Suffix <- factor( population$Prefix_Suffix )
population$SSLfinal_State <- factor( population$SSLfinal_State )
population$having_Sub_Domain <- factor( population$having_Sub_Domain )
population$Request_URL <- factor( population$Request_URL )

# Get a number of <sample.size> indices given a population size. 
phishing.sample.i <- sample( 1:(population.size), sample.size, replace = FALSE )
# With the above indices, get a training set sample and a test set sample of  
# phishing.
training_set <- population[ phishing.sample.i, ]
test_set <- population[ -phishing.sample.i, ]

# Generate model using predictors
phishing.model <- glm(
  Result ~ Prefix_Suffix + SSLfinal_State + having_Sub_Domain + Request_URL,
  family = binomial( link = "logit" ),
  data = training_set
)

# The accepted model is trained and 5 fold cross validation is used to 
# generalize the model
modelo <- train( Result ~ Prefix_Suffix + SSLfinal_State + having_Sub_Domain + Request_URL,
                 data = training_set,
                 method = "glm",
                 family = binomial( link="logit" ),
                 trControl = trainControl( method = "cv", number=5, savePredictions = TRUE))

# Model is evaluated
matrix <- confusionMatrix( modelo$pred$pred, modelo$pred$obs )
print( matrix )

# ROC Curve
# Shows our current model to determine its fidelity level.
probs_e <- predict( phishing.model, test_set, type="response" )
umbral <-0.5
preds_e <- sapply( probs_e, function( p ) ifelse ( p >= umbral,"1","-1" ) )
preds_e <- factor( preds_e, levels = levels( population[["Result"]] ) )
ROC_e <- roc ( test_set[["Result"]], probs_e )
plot( ROC_e )

matrix <- confusionMatrix( preds_e, test_set[["Result"]] )
print( matrix )

# Is this model trustful?
# Likelihood ratio test
print( anova( phishing.model, test = "LRT" ) )

# Independence
# This condition means that there must be no autocorrelation in the residuals.
cat("=== Durbin Watson Test ===\n")
print( durbinWatsonTest( phishing.model ) )

# Multicollinearity
# There must be no linear relation between two or more predictors.
cat("=== Multicollinearity ===\n")
cat("= Variance inflation factor =\n")
vif( phishing.model )
cat("= Tolerance statistics =\n")
print( 1 / vif( phishing.model ) ) 
cat("= VIF mean =\n")
mean(vif( phishing.model ))

# Obtain residuals and statistics
output <- data.frame( predicted.probabilities = fitted( phishing.model ) )
output [["standardized.residuals"]] <- rstandard( phishing.model )
output [[ "studentized.residuals" ]] <- rstudent( phishing.model )
output [[ "cooks.distance" ]] <- cooks.distance( phishing.model )
output [[ "dfbeta" ]] <- dfbeta( phishing.model )
output [[ "dffit" ]] <- dffits( phishing.model )
output [[ "leverage" ]] <- hatvalues( phishing.model )

# Evaluate standardized residuals out of normality range. 95% of standarized 
# residuals should be between -1.96 y 1.96, y 99% entre -2.58 y 2.58.
suspicious1 <- which( abs( output[["standardized.residuals"]] ) > 1.96 )
suspicious1 <- sort( suspicious1 )
# Residuals out of range.
print( rownames( training_set[suspicious1, ]) )

# Review cases with Cook's distance greater than one.
suspicious2 <- which( output[["cooks.distance"]] > 1)
suspicious2 <- sort( suspicious2 )
# Residuals with high Cook's distance.
print( rownames( training_set[suspicious2,]) )

# Review cases whose leverage is more than double or triple the average leverage.
mean.leverage <- ncol( training_set ) / 500
suspicious3 <- which( output [["leverage"]] > mean.leverage )
suspicious3 <- sort( suspicious3 )
# Residuals with a suspicious leverage, range is 0.062.
print( rownames( training_set[suspicious3,]) )

# Check cases with DFBeta greater than one.
suspicious4 <- which ( apply( output[["dfbeta"]] >= 1, 1, any ) )
suspicious4 <- sort( suspicious4 )
names( suspicious4 ) <- NULL
# Residuals with DFBeta greater than one.
print( rownames( training_set[suspicious4,]) )

# Detail of possibly atypical observations.
suspicious <- c( suspicious1, suspicious2, suspicious3, suspicious4 )
suspicious <- sort( unique( suspicious ) )

# Suspicious cases
print( training_set[suspicious,] )
print( output[suspicious, ] )

# Cross validation
# ROC Curve
probs_e <- predict( phishing.model, training_set, type="response" )
umbral <-0.5
preds_e <- sapply( probs_e, function( p ) ifelse ( p >= umbral,"1","0" ) )
preds_e <- factor( preds_e, levels = levels( population[["Result"]] ) )
ROC_e <- roc ( training_set[["Result"]], probs_e )
plot( ROC_e )

# Finalmente, viendo todas las pruebas hechas y las condiciones expuestas, 
# podemos concluir que el modelo ajustado y propuesto es un modelo aceptable.



### GAM utility.
### author: Tobi Seitz - tobi@tobitobi.de
### (c) 2018

library(mgcv)

### wraps a given predictor name with a smooth function
# 	p: name of the predictor (or anything you need smoothed)
# 	k: polynomial degree of smooth term. 
smoothPredictors <- function(p,k=NULL){
  if(is.null(k)){
    p <- paste0("s(",p,")")  
  }
  else {
    p <- paste0("s(",p,",k=",k,")")  
  }
  p
}

# if we use a smoothening function in a formula, we need to strip that from the column name later
# to get the original variable name. 
extractParameterFromSmoother <- function(x){gsub("\\)","",gsub("s\\(","",x))}

# This function helps avoiding smooth terms where linear modeling is possible.
# It checks a given model for estimated degrees of freedom (edf).
# If edf <= 1.1 (acknowledging rounding error), it uses that parameter as linear/parametric predictor
# If edf > 1.1 it is kept with the smooth function
# A new formula is derived and the GAM is calculated with that one
# example usage: lapply(listOfGAMs, simplifyGAM,d=myDataFrame);
#   model: GAM object (from gam() of the mgcv package)
#   d: data frame to run the new gam() function on. 
# returns: gam object
simplifyGAM <- function(model,d){
  mSummary <- summary(model) # gives us everything we need to re-do the model.
  # smoothed variables are the rownames of this table.
  mSmoothedFrame <- as.data.frame(mSummary$s.table);
  # parametric/linear variables are the rownames of this table
  mParametricFrame <- as.data.frame(mSummary$pTerms.table);
  
  # reponse variable / dependent 
  # attention: this currently fails if there is _MORE THAN ONE_ dependent variable.
  # we assume the first variable is the dependent.
  mResponse <- all.vars(model$formula)[1]

  # read the correct row names of our frames.
  mParametricRows <- rownames(mParametricFrame)
  # here we need to ensure that we only take the predictors that show only one degree of freedom.
  mLinearRows <- rownames(mSmoothedFrame[mSmoothedFrame$edf <= 1.1,])
  # these will be kept with the smooth function, because the edf are greater
  mCurveRows <- rownames(mSmoothedFrame[mSmoothedFrame$edf > 1.1,])

  # some magic to retrieve the original variable names. 
  mLinearPredictors <- sapply(mLinearRows, extractParameterFromSmoother)
  mLinearPredictors <- unname(mLinearPredictors) # for some reason the name persists... #TODO
  
  # compile the right hand of the formula by merging the three vectors.
  # Attention: The order is super important. 
  #   mParametricRows is likely to contain the name of a dichotomous nominal factor variable. 
  #   
  nRightHand <- c(mLinearPredictors, mCurveRows, mParametricRows)
  
  # concatenate the right hand with a "+""
  nFormulaString <- paste(nRightHand, collapse=" + ")
  # concatenate the two pieces with the tilde
  nFormulaString <- paste(mResponse,nFormulaString,sep=" ~ ")
  # final step: assemble the formula
  nFormula = as.formula(nFormulaString)
  # and make the gam.
  model <- gam(nFormula,data=d)
  # optional, but recommended:
  # add a pointer to the data to the model
  # visreg, e.g., needs this to extract residuals.
  model$data <- d
  model
}

# Gives a GAM based on a dependent variable, a vector of predictors and control variable, 
# and a vector of the control variables you want smoothed, so you can use it in lapply(..) and alike.
#   column: name of the column in d that holds outcome / response / dependent variable.
#   predictors: column names of predictor
#   controls: name of control variables
#   d: data frame (wide form) to run the new gam() function on. 
#   controls.smoothed: names of control variables that need smoothing.
#   k: maximum polynomial degree of smooth terms.
# returns: gam object
getGAM <- function(column, predictors, controls, d, k=NULL, select=FALSE, controls.smoothed = NULL, ...){
  # attention: to avoid weird collapses of the universe, make sure to have a continuous variable as first variable.
  # having a binary factor first breaks all kinds of things later.
  # you have been warned.
  smoothedPredictors <- lapply(predictors,smoothPredictors,k=k)
  smoothedControls <- lapply(controls, function(var){
    if(!is.null(controls.smoothed) & var %in% controls.smoothed){
      c <- smoothPredictors(var,k=k)
    } else {
      c <- as.character(var)
    }
    c
  })
  concatPredictors = paste(smoothedPredictors,collapse = "+")
  concatControls = paste(smoothedControls,collapse = "+")
  rightHand <- paste(concatPredictors, concatControls, sep = "+");
  autoFormula <- as.formula(paste(column,rightHand,sep = "~"))
  # see https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/gam.selection.html
  m <- gam(autoFormula, select = select, data=d, ...); # adding method="REML" results in less magic.  
  m
}

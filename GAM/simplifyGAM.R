### plot GAM
### author: Tobi Seitz - tobi@tobitobi.de
### (c) 2018

library(mgcv)

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
function(model,d){
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
  nRightHand <- c(mParametricRows, mLinearPredictors,mCurveRows)
  
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

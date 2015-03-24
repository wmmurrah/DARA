ittReg <- function(y=outcome,tx=tx.assign, x=covariates, 
                    data=df, ...){
  # Creates an intent-to treat model with the lm function. The outcome is 
  # assumed to be continuous and is scaled in the analysis.
  #
  # Args:
  #   y:         Character vector of length = 1 that is the column name of the 
  #              outcome variable.
  #   tx.assign: Character vector of length = 1 that is the column name of the 
  #              treatment assignment variable (randomized variable).
  #   x:         Character vector of any length that contains the column names  
  #              of the covariates.
  #   data:      The data frame containing the variables referenced by the 
  #              previous arguments.
  # Returns:
  #   An object of class 'lm'.
  # Create formula to be passed to 'lm'.
  fml <- as.formula(paste('scale(',y,')', ' ~ ', tx, ' + ', 
                          paste(x, collapse=' + ')))
  
  # Create lm model object.
  mod <- lm(fml, data=df)
  return(mod)
}
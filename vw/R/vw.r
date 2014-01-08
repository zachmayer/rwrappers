
#TODO:
#1. Multiclass loss
#2. Tests
#3. Comments/make a package
#4. call vw-varinfo perl script for coef
#5. Maybe don't allow character and numeric in the same namespace??

#' VW Control
#' 
#' This function builds a list of command line arguments for vowpal wabbit.  I'm lazy and haven't full documented this yet, so see vwHelp()
#' 
#' @param vw_path ...
#' @param final_regressor ...
#' @param cache_file
#' @param passes
#' @param compressed
#' @param loss_function
#' @param noconstant
#' @param l1
#' @param l2
#' @param quadratic
#' @param sort_features
#' @param audit
#' @param quiet
#' @param adaptive
#' @param exact_adaptive_norm
#' @param nonormalize
#' @param conjugate_gradient
#' @param bfgs
#' @param ... Additional parameters.  DANGER! If these are wrong, VW will crash
#' @export
#' @return A list of control parameters
#' @references
#' https://github.com/JohnLangford/vowpal_wabbit/wiki/Command-line-arguments
#' @examples 
#' vwControl()
vwControl <- function(
  vw_path=getOption('vw_path'),
  final_regressor=NULL,
  cache_file=NULL,
  passes=1,
  compressed=TRUE,
  loss_function='squared',
  noconstant=FALSE,
  l1=1,
  l2=1,
  quadratic=FALSE,
  sort_features=FALSE,
  audit=FALSE,
  quiet=FALSE,
  adaptive=FALSE,
  exact_adaptive_norm=FALSE,
  nonormalize=FALSE,
  conjugate_gradient=FALSE,
  bfgs=FALSE,
  ...){
  
  if(is.null(vw_path)){
    stop('Please set the path the the Vowpal Wabbit executable')
  }
  
  mainArgs <- list(
    vw_path=vw_path,
    final_regressor=final_regressor,
    cache_file=cache_file,
    passes=passes,
    compressed=compressed,
    loss_function=loss_function,
    noconstant=noconstant,
    l1=l1,
    l2=l2,
    quadratic=quadratic,
    sort_features=sort_features,
    audit=audit,
    quiet=quiet,
    adaptive=adaptive,
    exact_adaptive_norm=exact_adaptive_norm,
    nonormalize=nonormalize,
    conjugate_gradient=conjugate_gradient,
    bfgs=bfgs
    )
  otherArgs <- list(...)
  
  return(c(mainArgs, otherArgs))
}

#' Construct vw call
#' 
#' This function builds a string from the list of VW control parameters.  This string will be sent to the command line to call the VW algoryhtm.
#' 
#' @param control A list of control parameters for VW
#' @export
#' @return A string
#' @examples 
#' constructVWcall()
constructVWcall <- function(control=vwControl(help=TRUE)){
  
  #Dont make the path an argument
  vw_path <-  control$vw_path
  control$vw_path <- NULL
  
  #Remove FALSE arguments
  out <- lapply(control, function(x) {
    if(is.logical(x)) {
      if (!x){
        return(NULL)
      }
    }
    return(x)
  })
  
  #Remove NULL arguments
  out <- out[! sapply(out, is.null)]
  
  #Paste together flags plus their arguments
  out <- paste0('--', names(out), ' ', out)
  
  #Collapse to a single line
  out <- paste(out, collapse=' ')
  
  #Remove TRUE arguments
  out <- gsub('TRUE', '', out)
  
  #Paste on the VW path
  stopifnot(file.exists(vw_path))
  out <- paste(vw_path, out)
  return(out)
}

#' Fit a Vowpal Wabbit model
#' 
#' Given an iput dataset, this function caches the data and fits a vw model to the cached data.
#' 
#' @param y the target variable
#' @param X the data.frame of X variables.  Can include numeric and character data
#' @param case_weights the weights for each observations
#' @param namespaces the namespace to which each variable is assigned
#' @param file the previously-saved file of vowpal wabbit data
#' @param control the control parameters for the vw function
#' @export
#' @return An object of class VowpalWabbit
#' @examples
#' data(iris)
#' model <- VW(iris[,1], iris[,-1], control=vwControl(invert_hash=paste0(getOption('vw_cache'), '/model.text')))
#' cat(file=model$control$invert_hash)
#' unlink(model$control$final_regressor)
#' unlink(model$control$cache_file)
#' unlink(model$control$data)
#' unlink(model$control$invert_hash)
VW <- function(y=NULL, X=NULL, case_weights=NULL, namespaces=NULL, file=NULL, control=vwControl()){
  stopifnot(require(digest))
  
  #Todo: Formula interface
  
  from_saved_file <- !is.null(file)
  #Checks
  if (! is.null(y) & is.null(X)){
    stop('If y is provided, X must be provided')
  }
  if (is.null(y) & !is.null(X)){
    stop('If X is provided, y must be provided')
  }
  if (is.null(y) & is.null(X) & is.null(file)){
    stop('Please provide either y and X or a file')
  }
  if (!is.null(file)){
    warning('file method is in alpha, and no verification is done for the file.  Need to write verifyVWfile function')
  }
  
  #Assign namespaces
  if (is.null(namespaces) & (!from_saved_file)){
    #Default: all numeric in one namespace
    #Default: all factor/character in their own namespace
    namespaces <- colnames(X)
    namespaces[sapply(X, is.numeric)] <- 'numeric'
  }
  
  #Cache the VW data
  if(!from_saved_file){
    file <- cacheVW(y, X, case_weights=case_weights, namespaces=namespaces)
  }
  
  #Construct the rest of the control
  control$data <- file
  if(is.null(control$cache_file)){
    control$cache_file <- paste0(control$data, '.cache')
  }
  if(is.null(control$final_regressor)){
    control$final_regressor <- paste0(getOption('vw_cache'), '/', 'model-', digest(control))
  }
  
  #Fit the model
  call <- constructVWcall(control)
  time <- system.time(log <- system(call, intern=FALSE))
  
  #Construct a one-row dataset
  if (!from_saved_file){
    one_row <- oneRowDataset(y, X, namespaces)
  } else {
    one_row <- read.table(file, nrows=1, sep='\n', stringsAsFactors=FALSE)$V1
  }

  #Return a VW object
  out <- list(call=call, control=control, log=paste(log, collapse="\n"), 
              time=time, namespaces=namespaces, one_row=one_row)
  class(out) <- 'VowpalWabbit'
  return(out)
}

print.VowpalWabbit <- function(model){
  cat(model$log)
}

#' Update a vowpal wabbit model
#' 
#' This function starts with an existing vowpal wabbit model, and then runs some more passes
#' 
#' @param model an object of class VowpalWabbit
#' @param passes the number of additional passes to fit
#' @param final_regressor the path for saving the new model
#' @param readable_model the path to save the readable model (really the hashes)
#' @export
#' @return An object of class VowpalWabbit
update.VowpalWabbit <- function(model, passes=1, final_regressor=tempfile(), readable_model=NULL){
  
  #Todo: use a digest to choose the model, like in fitVW
  
  #Extract the control parameters
  control <- model$control
  
  #Checks
  stopifnot(file.exists(control$data))
  stopifnot(file.exists(control$final_regressor))
  
  #Use the old model as the initial regressor
  control$initial_regressor <- control$final_regressor
  control$final_regressor <- final_regressor
  control$readable_model <- readable_model
  control$passes <- passes
  
  #Fit the model
  call <- constructVWcall(control)
  time <- system.time(log <- system(call, intern=TRUE))
  
  #Return a VW object
  out <- list(call=call, control=control, log=paste(log, collapse="\n"), 
              time=time, namespaces=model$namespaces, one_row=model$one_row)
  class(out) <- 'VowpalWabbit'
  return(out)
}

#' Make predictions from a vowpal wabbit model
#' 
#' @param model an object of class VowpalWabbit
#' @param X the data to use for predicting
#' @param file the new data file for predicting
#' @param case_weights Case weights for the test set
#' @param predictions the file to save the predicitons to
#' @export
#' @return A vector or a matrix
predict.VowpalWabbit <- function(model, X=NULL, file=NULL, case_weights=NULL, predictions=tempfile(), ...){
  
  #Checks
  if(is.null(X) & is.null(file)){
    stop('A new X dataset or a new X file MUST be provided')
  }
  if((!is.null(X)) & (!is.null(file))){
    stop('Provide X or file, but not both')
  }
  stopifnot(file.exists(model$control$final_regressor))
  
  #Write data to a temp file
  if(!is.null(X)){
    y <- rep(1, nrow(X))
    file <- cacheVW(y, X, case_weights=case_weights, namespaces=model$namespaces)
  }
  
  #Construct control for the prediction
  control <- list(
    vw_path=getOption('vw_path'),
    testonly=TRUE,
    data=file,
    initial_regressor=model$control$final_regressor,
    predictions=predictions
  )
  
  #Make predictions
  call <- constructVWcall(control)
  print(call)
  log <- system(call, intern=TRUE)
  out <- read.table(control$predictions, header=FALSE)
  out <- rowMeans(out)
  return(out)
}

#' Show the coefficients of a VW model
#' 
#' #TODO: use new, "reversed hash" human readable model
#' 
#' @param model an object of class VowpalWabbit
#' @param data a file to save temp data to when getting the CFs
#' @param A file to save temp predictions to
#' @param cache_warning Warn about caching and cash inversion
#' @param verbose Whether to list issues as we try to read the CFs from a file
#' @export
#' @return A matrix
coef.VowpalWabbit <- function(model, data=tempfile(), predictions=tempfile(), cache_warning=TRUE, verbose=FALSE){
  stopifnot(require('data.table'))
  
  if(cache_warning){
    warning('Make sure you set cache=FALSE in the control, or the coefficients will not have names.  Set cache_warning=FALSE to turn this warning off')
  }
  
  #Add constant if needed
  if (! 'invert_hash' %in% names(model$control)){
    stop('Add invert_hash=TRUE to the control or --invert_hash=(/path/to/a/file) to the vw call to save human-readable coefficients')
  }
  stopifnot(require(stringr))
  
  #Load hashes with coefficients
  CF <- fread(model$control$invert_hash, header=FALSE, skip=12, sep=':')
  setnames(CF, c('Feature', 'Hash', 'Weight'))
  setkeyv(CF, 'Weight')
  CF <- as.data.frame(CF)
  return(CF)
}

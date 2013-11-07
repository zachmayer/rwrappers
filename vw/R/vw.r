
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

predict.VowpalWabbit <- function(model, X, case_weights=NULL, predictions=tempfile(), return_all=FALSE, ...){
  
  #Write data to a temp file
  stopifnot(file.exists(model$control$final_regressor))
  y <- rep(0, nrow(X))
  file <- cacheVW(y, X, case_weights=case_weights, namespaces=model$namespaces)
  
  #Construct control for the prediction
  control <- model$control
  control$data <- file
  control$cache_file <- paste0(control$data, '.cache')
  control$testonly <- TRUE
  control$initial_regressor <- control$final_regressor
  control$predictions <- predictions
  
  #Make predictions
  call <- constructVWcall(control)
  print(call)
  log <- system(call, intern=TRUE)
  out <- read.csv(control$predictions, header=FALSE)
  out <- out[,1]
  out <- matrix(out, nrow=nrow(X))
  if(!return_all){out <- out[,ncol(out)]} 
  return(out)
}

print.VowpalWabbit <- function(model){
  cat(model$log)
}

#TODO: use new, "reversed hash" human readable model
coef.VowpalWabbit <- function(model, data=tempfile(), predictions=tempfile()){
  
  #Add constant if needed
  if (! 'readable_model' %in% names(model$control)){
    stop('A human readable_model was not saved during fitting.  
         Try adding readable_model=tempfile() to the control.')
  }
  stopifnot(require(stringr))
  
  #Load hashes with coefficients
  CF <- read.csv(model$control$readable_model, stringsAsFactors=FALSE)
  CF <- CF[12:nrow(CF),]
  CF <- strsplit(CF, ':')
  CF <- data.frame(do.call(rbind, CF), stringsAsFactors=FALSE)
  colnames(CF) <- c('Hash', 'Weight')
  CF$Weight <- as.numeric(CF$Weight)
  out <- CF
  
  #Fit a 1-row model
  control <- list(
    vw_path=model$control$vw_path,
    initial_regressor=model$control$final_regressor,
    data=data,
    cache=TRUE,
    audit=TRUE,
    quiet=FALSE
  )
  write(model$one_row, data)
  call <- constructVWcall(control)
  log <- system(call, intern=TRUE)

  #Extract the feature-hash table
  split_log <-  unlist(strsplit(log, '\n|\t'))
  hash_lookup <- unlist(str_extract_all(split_log, '.*?\\^(.*?):.*?\\@.*?'))
  if(length(hash_lookup) >0){
    hash_lookup <- gsub('@', '', hash_lookup, fixed=TRUE)
    hash_lookup <- do.call(rbind, strsplit(hash_lookup, ':'))[,1:2]
    hash_lookup <- data.frame(hash_lookup, stringsAsFactors=FALSE)
    names(hash_lookup) <- c('Var', 'Hash')
    
    #Merge onto hashes
    out <- merge(hash_lookup, out, by='Hash', all=TRUE)
  }
  
  #Extract the constant
  Constant_lookup <- unlist(str_extract_all(split_log, 'Constant:.*?\\@.*?'))
  if(length(Constant_lookup) >0){
    Constant_lookup <- gsub('@', '', Constant_lookup, fixed=TRUE)
    Constant_lookup <- do.call(rbind, strsplit(Constant_lookup, ':'))[,1:2]
    Constant_lookup <- data.frame(t(Constant_lookup), stringsAsFactors=FALSE)
    names(Constant_lookup) <- c('Var', 'Hash')
    
    #Merge onto hashes
    if (Constant_lookup$Hash %in% out$Hash){
      out[out$Hash == Constant_lookup$Hash, 'Var'] <- 'Constant'
    }
  }
  
  out <- out[order(abs(out$Weight)),]
  return(out)
}

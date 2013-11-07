#' Display the vowpal wabbit help functions
#' 
#' This function prints the help for the VW executible
#' 
#' @param vw_path the path to the vw executible
#' @export
#' @return NULL
#' @examples 
#' vwHelp()
vwHelp <- function(vw_path=getOption('vw_path')){
  system(paste(vw_path, '-h'))
  return(invisible(NULL))
}

#' Some checks for the setup of the VW environment
#' 
#' This checks that the path to the vw executible is set and that a file exists.  It also checks that the VW cache has been set
#' 
#' @param vw_path the path to the vw executible
#' @export
#' @return NULL
#' @examples 
#' checkVW()
checkVW <- function(
  vw_path=getOption('vw_path'),
  vw_cache=getOption('vw_cache')){
  
  if(is.null(vw_path)){
    stop('Please set the vw_path using: options(vw_path="path/to/vw_executable")')
  }
  
  if(is.null(vw_cache)){
    stop('Please set the vw_path using: options(vw_cache="path/to/vw_cache")')
  }
  
  if(! file.exists(getOption('vw_path'))){
    stop('VW executible not found at ', getOption('vw_path'))
  }
  
  if(! file.exists(getOption('vw_cache'))){
    stop('Directory for VW cache not found at ', getOption('vw_cache'))
  }
  
  #Todo look for spaces in names
  
  #Todo test run of VW?
  
  return(invisible(NULL))
} 

#' Check a vowpal wabbit file
#' 
#' NOT IMPLEMENTED
#' 
#' @param x a file path
#' @return NULL
checkVWfile <- function(x){
  stop('NOT IMPLEMENTED')
}

#' Create a single row dataset with all possible VW variables
#' 
#' This function writes a single-row VW dataset, but includes all possible dumy variable levels.  It was intended for reversing VW hashes, but is NOW DEPRECIATED! It will be removed in future versions of this package
#' 
#' @param y 
#' @param X
#' @param namespaces
#' @return a single row data.frame
oneRowDataset <- function(y, X, namespaces){
  
  numeric_vars <- sapply(X, is.numeric)
  character_vars <- sapply(X, is.character) | sapply(X, is.factor)
  
  #Construct the namespaces
  stopifnot(length(namespaces)==ncol(X))
  namespace_list <- split(names(X), namespaces)
  out <- lapply(namespace_list, function(x){
    
    namespace_vars <- names(X) %in% x
    
    #Paste together numeric columns and values in that namespace
    out_num <- do.call(paste, lapply(names(X[,numeric_vars & namespace_vars,drop=FALSE]), 
                                     function(var) paste(var, X[1,var], sep=':')))
    
    #Paste together numeric columns and values in that namspace
    out_char <- paste(unlist(lapply(X[,character_vars & namespace_vars,drop=FALSE], unique)), collapse=' ')
    
    #Combine numeric and character columns
    if (length(out_num) > 0 & nchar(out_char) > 0){
      return(paste(out_num, out_char))
    } 
    if(length(out_num) > 0){
      return(out_num)
    }
    if(nchar(out_char) > 0){
      return(out_char)
    }
  })
  
  #Name the namespaces
  for (i in 1:length(out)){
    out[[i]] <- paste0('|', names(namespace_list)[i], ' ', out[[i]])
  }
  
  #Paste the namespaces together
  out <- do.call(paste0, out)

  #Paste on Y
  out <- paste(y[1], out)
  return(out)
}

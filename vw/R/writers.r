
#' Write Vowpal Wabbit data
#'
#' This function writes data in vowpal wabbit format
#'
#' @param y the target variable
#' @param X the data.frame of X variables.  Can include numeric and character data
#' @param case_weights the weights for each observations
#' @param namespaces the namespace to which each variable is assigned
#' @param file the file to write the data to.
#' @export
#' @return The path of the file
#' @references
#' http://stackoverflow.com/questions/11940573/r-convert-data-frame-to-input-file-improve-performance
#' @examples
#' data(iris)
#' f <- writeVWfile(runif(6), head(iris))
#' f
#' cat(readChar(f, file.info(f)$size))
#' unlink(f)
writeVWfile <- function(y, X, case_weights=NULL, namespaces=NULL, file=tempfile()){

  #Data checks
  stopifnot(is.null(dim(y)))
  stopifnot(is.data.frame(X))
  stopifnot(is.numeric(y) | is.factor(y) | is.character(y))

  #Check X datatypes
  numeric_vars <- sapply(X, is.numeric)
  character_vars <- sapply(X, is.character) | sapply(X, is.factor)
  stopifnot(all(numeric_vars | character_vars))

  #Convert factors to numeric
  if(is.factor(y)){
    if (length(levels(y)) < 2){
      stop('y must have at least 2 levels')

    } else if (length(levels(y)) > 2){
      #stop('Writing multiclass problems are not yet supported')
      y <- as.numeric(y) + 1

    } else if (length(levels(y)) == 2){
      y <- as.numeric(y)
      y[y==min(y)] <- -1
      y[y==max(y)] <- 1

    } else {
      stop('HTF Did you get HERE?')
    }
  }

  #Check character columns
  if(any(character_vars)){
    bad_format <- sapply(X[,character_vars,drop=FALSE], function(x) any(grepl(':|\\||\n', x)))
    if(any(bad_format)){
      stop('The following column(s) are badly formatted: ', paste(names(bad_format[which(bad_format)]), collapse=', '),
           '. Please replace ":",  "|", and "\\n" characters')
    }
  }

  #Assign namespaces
  if (is.null(namespaces)){
    #Default: all numeric in one namespace
    #Default: all factor/character in their own namespace
    namespaces <- colnames(X)
    namespaces[numeric_vars] <- 'numeric'
  }

  #Construct the namespaces
  stopifnot(length(namespaces)==ncol(X))
  namespace_list <- split(names(X), namespaces)
  out <- lapply(namespace_list, function(x){

    namespace_vars <- names(X) %in% x

    #Paste together numeric columns and values in that namespace
    out_num <- do.call(paste, lapply(names(X[,numeric_vars & namespace_vars,drop=FALSE]), function(var) paste(var, X[,var], sep=':')))

    #Paste together numeric columns and values in that namspace
    out_char <- do.call(paste, X[,character_vars & namespace_vars,drop=FALSE])

    #Combine numeric and character columns
    if (length(out_num) > 0 & length(out_char) > 0){
      return(paste(out_num, out_char))
    }
    if(length(out_num) > 0){
      return(out_num)
    }
    if(length(out_char) > 0){
      return(out_char)
    }
  })

  #Name the namespaces
  for (i in 1:length(out)){
    out[[i]] <- paste0('|', names(namespace_list)[i], ' ', out[[i]])
  }

  #Paste the namespaces together
  out <- do.call(paste0, out)

  #Paste on weights
  if (! is.null(case_weights)){
    stopifnot(length(case_weights) == nrow(X))
    stopifnot(all(is.finite(case_weights)))
    stopifnot(all(case_weights > 0))
    out <- paste(case_weights, out)
  }

  #Paste on Y
  out <- paste(y, out)

  #Write the file
  write.table(data.frame(x=out), file=file, sep='', row.names=FALSE, col.names=FALSE, quote=FALSE, na = "NA", dec = ".")

  #Return the file path
  return(file)
}

#' Write Vowpal Wabbit data to the VW cache
#'
#' This function writes data in vowpal wabbit format to the VW cache folder
#'
#' @param y The target variable
#' @param X The data.frame of X variables.  Can include numeric and character data
#' @param case_weights The weights for each observations
#' @param namespaces The namespace to which each variable is assigned
#' @param run_checks Whether to run the vw check function
#' @export
#' @return The path of the file
#' @references
#' http://stackoverflow.com/questions/11940573/r-convert-data-frame-to-input-file-improve-performance
#' @examples
#' data(iris)
#' f <- cacheVW(runif(6), head(iris))
#' f
#' cat(readChar(f, file.info(f)$size))
#' unlink(f)
cacheVW <- function(y, X, case_weights=NULL, namespaces=NULL, run_checks=TRUE, vw_cache=getOption('vw_cache')){
  require('digest')
  require('data.table')

  if(run_checks) checkVW(vw_cache=vw_cache)

  stopifnot(is.data.frame(X))
  data_hash <- digest(c(digest(y), sapply(X, digest), digest(case_weights), digest(namespaces)))

  hash_table_path <- paste(vw_cache, '.VW_hash_table.RData', sep='/')

  if(file.exists(hash_table_path)){
    load(hash_table_path)
  } else {
    hash_table <- data.table(hash='', key='hash')
  }

  hash_lookup <- hash_table[hash==data_hash,]$hash
  data_path <- paste0(getOption('vw_cache'), '/', data_hash, '.txt')

  if(length(hash_lookup)==0){
    sink <- writeVWfile(y, X, case_weights=case_weights, namespaces=namespaces, file=data_path)
    hash_table <- rbind(hash_table, data.table(hash=data_hash))
    save(hash_table, file=hash_table_path)
  }

  return(data_path)

}


#' Format a data.table in vowpal wabbit format
#'
#' This function formats a data.table in vowpal wabbit format (with namespaces).
#'
#' @param x a data.table (or object coerceable to data.table)
#' @param target_var (optional) the y variable for supervised vowpal wabbit models
#' @param weight_var (optional) the weights variable for supervised vowpal wabbit models
#' @param namespaces a list of namespaces and the variables in them.  By default this function builds 1 namespace
#' @param out_digits How many digests to round numbers to before writing
#' @param verbose Print a log as the function runs
#' @export
#' @return A character vector
data_table_to_vw <- function(x, target_var=NULL, weight_var=NULL, namespaces=NULL, out_digits=8, verbose=TRUE){
  #Load Libraries
  library('data.table')
  library('digest')

  #Check class
  if(!is.data.table(x)){
    x <- data.table(x)
  }

  #Check names
  stopifnot(target_var %in% names(x))
  if(!is.null(weight_var)){
    stopifnot(weight_var %in% names(x))
    stopifnot(is.numeric(x[[weight_var]]))
  }
  if(any(is.null(names(namespaces)))){
    stop('All namespaces must be named.')
  }
  if(any(names(namespaces)=='')){
    stop('All namespaces must have at least one letter in their name.')
  }
  if(any(duplicated(names(namespaces)))){
    stop('Namespaces cannot have duplicate names')
  }
  if(any(names(namespaces)==' ')){
    stop('No namespace can have a single space as the name')
  }

  #Check NAs
  if(!is.null(target_var)){
    if(any(is.na(x[[target_var]]))){
      stop('Target cannot contain NAs')
    }
  }
  if(!is.null(weight_var)){
    if(any(is.na(x[[weight_var]]))){
      stop('Weight variable cannot contain NAs')
    }
  }

  #Make default namespace
  if(is.null(namespaces)){
    namespaces <- list(
      d=setdiff(names(x), c(target_var, weight_var))
    )
  }

  #Check for stupid target leakage
  xvars <- unlist(namespaces)
  if(any(xvars==target_var)){
    stop('Target variable cannot also be in a namespace')
  }

  #Check for duplicates in namespaces
  dups <- xvars[duplicated(xvars)]
  xvars <- unique(xvars)
  if(length(dups)>0){
    stop(paste('The following columns are in multiple namespaces:', paste(dups, collapse=', ')))
  }

  #Check for missing vars
  missing_columns <- ! xvars %in% names(x)
  if(any(missing_columns)){
    stop(
      paste(
        'The following columns are in namespaces but are missing from x:',
        paste(xvars[missing_columns], collapse=', ')
      )
    )
  }

  #Remove non-used variable
  non_data_columns <- setdiff(names(x), c(target_var, weight_var, xvars))
  for(var in non_data_columns){
    set(x, j=var, value=NULL)
    gc(reset=TRUE)
  }

  #To character
  non_char <- names(x)[!sapply(x, is.character)]
  non_char <- setdiff(non_char, c(target_var, weight_var))
  for(var in non_char){
    nas <- which(is.na(x[[var]]))
    if(is.numeric(x[[var]])){
      set(x, j=var, value=paste0(' ', var, ':', as.character(round(x[[var]], out_digits))))
    } else{
      set(x, j=var, value=paste0(' ', as.character(x[[var]])))
    }
    gc(reset=TRUE)
    set(x, i=nas, j=var, value='')
    gc(reset=TRUE)
  }
  rm(var)

  #Target to character
  if(!is.null(target_var)){
    set(x, j=target_var, value=as.character(x[[target_var]]))
  }

  #Weight to character
  if(!is.null(weight_var)){
    set(x, j=weight_var, value=paste0(' ', as.character(round(x[[weight_var]], out_digits))))
  }

  #Build separator columns for namespaces
  output_column_order <- c(target_var, weight_var, xvars)
  setcolorder(x, output_column_order)
  for(n in names(namespaces)){
    new_column_name <- digest(n)
    first_col_in_namespace <- which(output_column_order==namespaces[[n]][[1]])
    output_column_order <- append(output_column_order, new_column_name, after=first_col_in_namespace-1)
    set(x, j=new_column_name, value=paste0(' |', n))
  }
  setcolorder(x, output_column_order)

  #Construct output vector
  out <- do.call(paste0, x)
  return(out)
}





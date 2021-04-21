#-------------------------------------------------------------------------#
arr2df <- function(.arr, dfcols = NULL, dfval = "val"){
  
  arrdim <- dim(.arr)
  ndim <- length(arrdim)
  nameddims <- !sapply(dimnames(.arr), is.null)
  dimNames <- if(is.null(dimnames(.arr))) list() else dimnames(.arr)
  
  # add names to missing dimensions
  if(is.null(dimnames(.arr))){       # add 1:n if missing all dimensions
    for(i in 1:ndim){
      dimNames[[i]] <- 1:arrdim[i]
    }
  } else {
    for(i in which(!nameddims)){     # add 1:n for missing dimensions only
      # i <- 2
      # dimNames[[i]] <- paste("D", i, "-", 1:arrdim[i], sep = "")
      dimNames[[i]] <- 1:arrdim[i]
    }
  }
  
  # make ordered dataframe using expand.grid()
  .df <- expand.grid(dimNames)
  
  # re-name columns if specified in dfcols
  if(!is.null(dfcols) & length(dfcols) == ncol(.df)){
    names(.df) <- dfcols
  } else {
    names(.df) <- sprintf("d%s", 1:ndim)
  }
  
  # add values to dataframe
  .df[[dfval]] <- as.vector(.arr)
  
  return(.df)
}
#-------------------------------------------------------------------------#
df2arr <- function(.df, drop = F, fill = NA){
  
  #-------------------------------------------------------------------------#
  # function to convert dataframe into array
  #   - dataframe must be composed only of factor and numeric classes (could be modified to handle character classes)
  #   - factors and their levels (including empty levels) are used to determine array dimensions and names
  #   - order of factor variables should determine array dimensions (including factors with only single levels)
  #-------------------------------------------------------------------------#
  
  # drop empty levels
  if(drop){.df <- droplevels(.df)}
  
  isfac <- sapply(.df, is.factor)
  
  facnames <- names(.df)[isfac]
  valnames <- names(.df)[!isfac]
  
  lvls <- sapply(.df[facnames], levels)
  nvals <- sum(!isfac)
  
  # check for extra classes in dataframe
  if(!all(sapply(.df[!isfac], is.numeric))) stop("function only takes dataframe with factors and numeric classes; check input dataframe")
  
  # warn if dataframe has more rows than the product of the dimensions (i.e. duplicated values)
  if(nrow(.df) > prod(sapply(lvls, length))) stop("dataframe has more rows than expected (i.e. product of array dimensions); each row should specify a single value")
  
  # create array with same dimensions set by levels of factor
  .arr <- array(fill, c(nvals, sapply(lvls, length)), c(list(names(.df)[!isfac]), lvls))
  
  for(j in 1:nvals){
    # j <- 1
    .arr[cbind(j,sapply(.df[facnames], as.numeric))] <- .df[!isfac][,j]  
  }      
  return(.arr)
}
#-------------------------------------------------------------------------#
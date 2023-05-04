lazyExpandGrid <- function(...) {
  dots <- list(...)
  dotnames <- names(dots)
  if (is.null(dotnames)) {
    dotnames <- paste0('Var', seq_along(dots))
  }
  dotnames <- make.unique(make.names(dotnames))
  names(dots) <- dotnames
  sizes <- lengths(dots)
  indices <- cumprod(c(1L, sizes))
  final. <- preset <- FALSE
  numfactors <- length(indices)
  maxcount <- unname(indices[ length(indices) ])
  i <- 0
  env <- environment()
  
  nextItem <- function(index) {
    if (missing(index)) {
      li <- length(i)
      if (preset) {
        if (li > 1) i <<- i[-1L]
        thisi <- i[[1L]]
        preset <<- (li > 2)
      } else {
        if (final.) return(NULL)
        thisi <- (i <<- i + 1)
      }
    } else {
      env$setIndex(index)
      return(env$nextItem())
    }
    if (thisi > maxcount || i < 1L) return(NULL)
    structure(setNames(Map(`[[`, dots, (thisi - 1L) %% indices[-1L] %/% indices[-numfactors] + 1L),
                       dotnames),
              row.names = as.character(thisi), class = 'data.frame')
  }
  
  nextItems <- function(index) {
    if (! missing(index)) {
      env$setIndex(index)
      return(env$nextItems())
    }
    li <- length(i)
    if (li > 1) {
      rn <- as.character(i[-1L])
      ret <- do.call(rbind.data.frame, Filter(Negate(is.logical),
                                              lapply(2:length(i), function(ign) env$nextItem())))
      rownames(ret) <- rn
      ret
    } else {
      env$nextItem()
    }
  }
  
  setIndex <- function(index, append. = FALSE, final. = FALSE) {
    isgood <- (index > 0) & (index <= maxcount)
    if (! any(isgood)) {
      stop(sprintf("'index' must have at least one positive integer no more than the design space size (%s)",
                   maxcount),
           call. = FALSE)
    }
    if (! all(isgood)) {
      warning('non-positive or too-high indices are invalid, ignored', call. = FALSE)
      index <- index[isgood]
    }
    i <<- c(if (append.) i else i[[1L]], index)
    preset <<- TRUE
    final. <<- final.
  }
  
  getIndex <- function() return(i[[1L]])
  
  getIndices <- function() return(i)
  
  getNextIndex <- function() if (length(i) > 1) i[[2L]] else i+1
  
  l <- list(nextItem = nextItem, nextItems = nextItems,
            getIndex = getIndex, getIndices = getIndices,
            setIndex = setIndex, getNextIndex = getNextIndex,
            n = maxcount, factors = dots
  )
  class(l) <- c(class(l), 'lazyExpandGrid')
  l
}
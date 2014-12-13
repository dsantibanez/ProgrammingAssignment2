## This function gets and sets the value of a matrix and gets and sets the value of the inverse of that matrix
makeCacheMatrix <- function(x = matrix()) {
   m <- NULL                                ## initialize objet m
   set <- function(y) {                     ## function to set the value of m
      x <<- y
      m <<- NULL                            ## sets the global value NULL to m
   }
   get <- function() x                      ## function that gets m
   setsolve <- function(solve) m <<- solve  ## function calculates the inverse of the matrix and store it globally
   getsolve <- function() m                 ## get cached value of the inverse matrix stored in setsolve
   list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)  ## list elements
}

## This function returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
   m <- x$getsolve()                        ## m stores the inverse matrix
   if(!is.null(m)) {                        ## if inverse matrix is previously calculated
      message("getting cached data")        ## print message
      return(m)                             ## gets the inverse matrix stored in cache
   }
   data <- x$get()                          ## gets the matrix
   m <- solve(data, ...)                    ## calculates for the first time its inverse
   x$setsolve(m)                            ## stores the value of the just calculated inverted matrix in cache
   m                                        ## print matrix
}

## Returns inverse of matrix; input matrix must be inverse-able
## uses caching to avoid repeated calculations and improve performance

## Takes the input of a matrix, returns the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
    result_matrix <- NULL
    set <- function(y) { ## Sets the matrix to input; result variable to null
      x <<- y
      result_matrix <<- NULL
    }
    get <- function() ## used to get the input variable
    {
      x
    }
      
    setinverse <- function(mat_output) 
    {
      ## used to set the inverted output 
      ## to the output variable
      result_matrix <<- mat_output
    }
      
    getinverse <- function() ## used to get the resulting matrix from the cache function
    {
      result_matrix
    }
    result_matrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }
  



## This function returns the inverse of the input matrix 
## If the inverse was previously calculated, returns from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## Gets the value from the getinverse function
    result_matrix <- x$getinverse()
    if(!is.null(result_matrix)) { ## checks if the value was already calculated
      ## if yes, return result from cache
      message("getting cached data")
      return(result_matrix)
    }
    ## if valye doesn't exist already, calculate using solve() function and return result
    data <- x$get()
    result_matrix <- solve(data, ...)
    x$setinverse(result_matrix)
    result_matrix
  }
    


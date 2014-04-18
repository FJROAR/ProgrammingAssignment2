## This program compute the inverse of a matriz and it verifies in
##this one has been computed, in this case, it returns the above
##value

##I make in almost all, a copy paste of the example R program


## This function creates a special "matrix", which is really a list containing a function to
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse
##get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  ##Here is the principal difference, we change mean by solve
  ##Where we put mean, now, we puto xxxinverse in order to do 
  ##the variable more explainable
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##This function calculates the inverse of the special "matrix" created with the above function. It first checks 
##to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips 
##the computation. Otherwise, it calculates the inverse of the data and sets the value of the inverse in the 
##cache via the setmean function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

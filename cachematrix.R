## makeCacheMatrix returns a list with 4 functions.
## The environment of the list also contains two variables:
## m, a matrix, and inv, the inverse of matrix m.
## This function makeCacheMatrix should be called first.
## Note that the matrix whose inverse is desired may
## optionally be passed to makeCacheMatrix.
## The returned list can be used to get and set the matrix m
## or get or set the the inverse of m.
## Additionally, the returned list may be passed to
## cacheSolve which either computes the inverse
## or, if the inverse of m has already been computed,
## the cached inverse of m.
makeCacheMatrix <- function(m = matrix())
{
  inv <- NULL
  set <- function(y)
  {
    # update m and inv only if the passed value y
    # differs from the value of m
    if (!identical(m,y))
    {
      m <<- y
      inv <<- NULL
    }  
  }
  get <- function() m
  setinv <- function(i) inv <<- i
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


##
## cacheSolve does the hard work of computing the 
## the inverse, if not already computed, or
## returning the cached inverse, if the inverse
## has already be computed.
## It is important to note that cacheSolve
## expects as its sole argument a list which
## is returned from makeCacheMatrix.
##
cacheSolve <- function(x)
{
  inv <- x$getinv()
  #
  # It is best to have a single point of
  # return from a function.  Using an 
  # if-then-else ensures a single point of return
  #
  if(!is.null(inv))
  {
    message("getting cached inverse")
    inv
  }
  else
  {
    message("calling solve()")
    x$setinv(solve(x$get()))
    x$getinv()
  }    
}
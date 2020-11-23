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
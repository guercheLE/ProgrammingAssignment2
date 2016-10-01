## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function caches x and solve (inverse matrix) and return a list of functions which
## let get or set them
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function
## This function gets solve (inverse matrix). If it is not null return solve cached,
## otherwise, get data (x), solve it, save it and return it
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the solve (inverse) of 'x'

  ## Get cached solve (inverse matrix)
  s <- x$getsolve()
  ## Return cached solve in case it is not null
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  ## otherwise, get data (x)
  data <- x$get()
  ## solve (inverse matrix) data (x)
  s <- solve(data, ...)
  ## save/store calculated s (inverse matrix)
  x$setsolve(s)
  ## return calculated s (inverse matrix)
  s
}

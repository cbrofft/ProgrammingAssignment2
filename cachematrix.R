## MAKE VECTOR THAT SETS, GETS VALUE/MEAN
makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,setmean = setmean, getmean = getmean)
}
## FUNCTION TO CACHE MEANS IF THEY ARE NULL
cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

## FUNCTION TO CREATE SPECIAL VECTOR THAT CACHES THE INVERSE OF MATRIX
makeCacheMatrix <- function(x = matrix())
{
  invert <- NULL
  set <- function(y){
    x <<- y
    invert <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invert <<- inverse
  getinverse <- function() invert
  list( set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}
## CALCULATES THE INVERSE OF THE SPECIAL MATRIX CREATED
cacheSolve <- function(x, ...)
{
  invert <- x$getinverse()
  if(!is.null(invert)){
    message("retrieving cache of data")
    return(invert)
  }
  matr <- x$get()
  invert <- solve(matr, ...)
  x$setinverse(invert)
  invert
}
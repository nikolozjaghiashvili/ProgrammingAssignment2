makeMatrix <- function(x) {
  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) I <<- solve
  getInverse <- function() I
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheInverse <- function(x, ...) {
  I <- x$getInverse()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  data <- x$get()
  I <- solve(data)
  x$setInverse(I)
  I
}

x=matrix(c(-1,1,1.5,-1),nrow=2,ncol=2)
y=makeMatrix(x)
cacheInverse(y)
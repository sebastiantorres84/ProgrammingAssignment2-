
## we create an object, an array
## which stores cache in its inverse and
## stores an array and caches its inverse.
makeCacheMatrix <- function(x = matrix()){
  inver <- NULL
  set <- function(y){
    x <<- y
    inver <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inver <<- inverse}
  getInverse <- function() {inver}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
########################
#now, we are going to calculate the inverse of the matrix created by makecachematrix
#we recover the inverse in case it has not changed
cacheSolve <- function(x, ...){
  inver <- x$getInverse()
  if(!is.null(inver)){
    message("getting cached")
    return(inver)
  }
  mat <- x$get()
  inver <- solve(mat, ...)
  x$setInverse(inver)
  inver
}

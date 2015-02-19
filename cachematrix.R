## Function pair makeCacheMatrix and cacheSolve caches the inverse of a square, invertible matrix

## makeCacheMatrix is a function that takes a square invertible matrix (e.g. determinant cannot be zero) as its input and returns a list containing four elements (all functions). Elements of the list are accessed using $-sign when running the cacheSolve function. "set" is not called in cacheSolve but can be used to modify/change the invertible matrix without having to run the whole makeCacheMatrix function again by calling it with x$set(y). x <<- y then overwrites existing matrix with new matrix and inv <<- overwrites cached matrix inversion and sets "inv" to NULL so that "getting cached matrix"-message is not triggered in cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve takes a list created by running makeCacheMatrix (or a list modified using x$set(y)) as its input. The function at first tries to find an existing inversed matrix (x$getinverse()). If one already exists a message ("getting cached matrix") is displayed, the cached matrix is returned and the function exited. If get$inverse() returns NULL the inverse of the input matrix is calculated with the "solve"-function. Calling x$setinverse(inv) then overwrites the "inv"-variable in makeCacheMatrix with the newly inversed matrix. Lastly the inversed matrix is returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {                   
    message("getting cached matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
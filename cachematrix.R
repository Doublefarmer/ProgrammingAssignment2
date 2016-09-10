## The two functions allow for caching the inverse of a matrix in order to avoid repeated computation of the inverse. 
## By applying the rules of lexical scoping, objects created in the first function serve as input 
## for the second function in which the inverse is computed and set in the cache (if not cached already).
## Please note: It is assumed that the matrix supplied is always invertible.

## The first function creates a special "matrix" object that can cache its inverse. It allows access to any objects 
## defined in the environment of the original function by creating objects of type list (), specifically "getters" and "setters". 
## The "getters" and "setters" retrieve/set data within an object and can be used by subsequent code to access the values of 
## x (=the matrix) or i (= the inverse of the matrix):
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve() is able to calculate and store the inverse for the input argument of type makeCacheMatrix().
## The function checks if the inverse has already been calculated. If so, it gets the inverse from the cache. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

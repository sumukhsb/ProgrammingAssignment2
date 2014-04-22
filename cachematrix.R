## makeCacheMatrix is used to build a set of disjoint caches and cacheSolve is used for
## fetching the cached value for each cache. For example, if A=matrix(1:4,2,2) and B=matrix(2:5,2,2)
## occur very frequently in the matrix, we can create two different caches for them using makeCacheMatrix().
## cacheSolve can then be used to fetch the cached values from these two different caches
##
##> a<-makeCacheMatrix(A)   <--- Build a new cache for A
##> b<-makeCacheMatrix(B)   <--- Build a new cache for B
##> cacheSolve(a)           <--- Fetch from A's cache
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> cacheSolve(b)           <--- Fetch from B's cache
##[,1] [,2]
##[1,] -2.5    2
##[2,]  1.5   -1
##> cacheSolve(b)           <--- Fetch again from B's cache
##getting cached data
##[,1] [,2]
##[1,] -2.5    2
##[2,]  1.5   -1

## makeCacheMatrix is used to build a set of disjoint caches and provides a list of
## functions for setting and getting the corresponding cache values. Because of lexical scoping,
## these caches are unique to each function definition and can be manipulated
## within the function using the <<- operator.
## Input: square matrix

makeCacheMatrix <- function(x = matrix()) {

  # Force x into becoming a matrix of numbers if its isn't
  x<-matrix(as.numeric(as.vector(x)),nrow(x),nrow(x))
  
  # cacheMatrix is a free variable acting as cache of matrix inverse
  # Initializing it as NULL
  cacheMatrix <- NULL              
  
  # SET FUNCTION: setting the input matrix in the environment.
  # Also setting cacheMatrix as NULL, which has to be set again using setinverse()
  set <- function(y) {
    x <<- y
    cacheMatrix <<- NULL
  }
  
  # GET FUNCTION: getting the input matrix from the environment.
  get <- function() x
  
  # SETINVERSE & GETINVERSE: setting and getting the cached inverse value of input matrix.
  setinverse <- function(inverse) cacheMatrix <<- inverse
  getinverse <- function() cacheMatrix
  
  # Building the list of functions to be returned
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve function fetches the cached value of matrix inverse by making use
## of cacheFunctions (list of matrix caching functions). If the cached value is not set,
## it freshly computes the matrix inverse, caches it and returns the value.
## Input: cacheFunctions object

cacheSolve <- function(x, ...) {
  
  # Fetch the cached matrix inverse from cache using getinverse()
  inverse <- x$getinverse()

  # Return the cached value if cache is not empty
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  # Else get the input matrix, compute its inverse using solve()
  # and store it in the cache using setinverse()
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse                   # Return the calculated inverse
}
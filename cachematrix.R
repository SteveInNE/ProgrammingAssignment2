## Using two functions it is possible to evaluate the performance of caching the results of determining the inverse of a square matrix. 
## makeCacheMatric is used to load the initial matrix in cache and sets the inverse to NULL. 
## cacheSolve is used to determine it the inverse is in cache if not, the inverse is solved and stored in cache. 
##
## Note - Each time the makeCacheMatrix function is called, the cache is reset.

## This is the makeCacheMatrix Function. 
## This function configures a set of functions to set and evaluate the contents of the cache for the matrix input. 
## The set function loads the initial matrix into cache.

makeCacheMatrix <- function(x = matrix()) {
  ## x: is a square matrix that is dtinvertable
  ## return: a list containing four(4) functions that
  ##      - set the matrix in cache
  ##      - get the matrix in cache
  ##      - set the inverse matrix in cache
  ##      - get the inverse matrix in cache
  ## These functions are used as the input to cacheSolve() function.
  
  inv = NULL
  set = function(y) {
    # the `<<-` is used to assign a value to an object in an  
    # environment different from the current environment. 
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## This is the cacheSolve Function. 
## This function uses the functions defined by makeCacheMatrix to determin if the matrix is in cache. 
## The function uses the “solve” function to determine the inverse matrix.

cacheSolve <- function(x, ...) {
  ## x: is the input matrix
  ## inv: inverse of x
  ## cacheSolve uses the functions created by makeCacheMatrix (get, getinv, setinv) to determine if the matrix inverse is in cache.
  
  inv = x$getinv()
  
  # if the inverse has already been calculated
  if (!is.null(inv)){
    # get it from the cache and skips the computation. 
    message("using cached data")
    return(inv)
  }
  
  # otherwise, calculates the inverse 
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setinv(inv)
  return(inv)
}

## This is a test harness to evaluate the performance of set/get and recomputing the inverse.

matTestHarness <- function(matIn){
  ## matIn: an invertible matrix
  ## matInInv: the inverse of the matIn
  
  temp = makeCacheMatrix(matIn)
  
  start.time = Sys.time()
  matInInv = cacheSolve(temp)
  lapseTime = Sys.time() - start.time
  print(lapseTime)
  
  ## Repeat using cache. What is the time difference?
  start.time = Sys.time()
  matInInv = cacheSolve(temp)
  lapseTime = Sys.time() - start.time
  print(lapseTime)
  return(matInInv)
}

## Below is sample code to exercise the test harness.
"
set.seed(2175712)
r = sample(1:100)
matTest = matrix(r, nrow=10, ncol=10)
invmatTest = matTestHarness(matTest)
matTestInv <- solve(matTest)
checkAns = matTest %*% invmatTest
print(matTest)
print(invmatTest)
print(abs(checkAns), digits=0)
checkAns = matTest %*% matTestInv
print(matTest)
print(matTestInv)
print(abs(checkAns), digits=0)
"

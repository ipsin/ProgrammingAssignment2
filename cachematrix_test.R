source("cachematrix.R")

require(RUnit)

testCaching <- function() {
  require(RUnit)
  
  # Keep track of when the calc_func is called with the gen_called flag.
  gen_called <<- FALSE
  calc_func <- function(x) {
    gen_called <<- TRUE
    return (solve(x))
  }
  
  x <- makeCacheMatrix(matrix(c(2,1,0,4), nrow=2))
  
  # See that the cache function is called the first time
  checkTrue(!gen_called)
  result <- x$cacheGet("fake", calc_func)
  checkTrue(gen_called)
  checkEquals(solve(x$get()), result)
  
  gen_called <- FALSE
  result <- x$cacheGet("fake", calc_func)
  checkTrue(!gen_called)
  checkEquals(solve(x$get()), result)
}

testInverse <- function() {
  x <- makeCacheMatrix(matrix(c(3,2,-2,5), nrow=2))
  identity <- matrix(c(1,0,0,1), nrow=2)
  checkEquals(identity, x$get() %*% cacheSolve(x))
}

testCacheCheck <- function() {
  x <- makeCacheMatrix(matrix(c(3,2,-2,5), nrow=2))
  checkTrue(is.null(x$cacheGet("foo")))
}

testCaching()
testInverse()
testCacheCheck()


## The function makeCacheMatrix creates a special "matrix" object that can cache its inverse.
##

makeCacheMatrix <- function(inMatrix = matrix()) {

  #Initialize the parent environment variable that is used a cache object
  invCachedMatrix <- NULL

  #Whenever there is a new matrix passed using setMatrix, clear out the cache object in the parent environment
  setMatrix <- function(y) {
    inMatrix <<- y
    invCachedMatrix <<- NULL
  }

  #Return the Original Matrix if getMatrix is invoked
  getMatrix <- function() inMatrix

  #Set the invCachedMatrix using << operator.
  #So check for the variable invCachedMatrix in the parent environment rather than in the local environment.
  setInvertedCachedMatrix <- function(inInvertedMatrix) invCachedMatrix <<- inInvertedMatrix

  #Return the Inverted Cached Matrix regardless of its value
  getInvertedCachedMatrix <- function() invCachedMatrix

  #Return a list object with our special functions
  list(
    setMatrix = setMatrix,
    getMatrix = getMatrix,
    setInvertedCachedMatrix = setInvertedCachedMatrix,
    getInvertedCachedMatrix = getInvertedCachedMatrix
  )
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
##   then the cacheSolve should retrieve the inverse from the cache.
##
## This function cacheSolve assumes the matrix is always invertible
## The input to this function is a special list returned by makeCacheMatrix function

cacheSolve <- function(x, ...) {

  #Step 1. Get the cached Matrix from the special list provided by makeCacheMatrix function

  m <- x$getInvertedCachedMatrix()

  #Step 2. Check if the returned Matrix is NOT NULL which means the data is cached
  #         and return the value and terminate the function.

  if(!is.null(m)) {
          message("getting cached matrix")
          return(m)
  }

  #Step 3. If we pass step 2, then the inverse is not calculated yet. So get the Original Matrix Data

  data <- x$getMatrix()

  #Step 4. This step will only be called once.
  #        Find the Inverse of a Matrix using solve function.
  #        Store the inverted value into the original list (x)
  #        also return the computed value.

  m <- solve(data)
  x$setInvertedCachedMatrix(m)
  m
}

## How this works? See for yourself with this little example run.

## source the file first
# > source("cachematrix.R")

## create a sample matrix of 2x2
# > m1<-matrix(rnorm(1:4), nrow=2, ncol=2)
## See the value

# > m1
#         [,1]     [,2]
#[1,] 1.374692 2.029445
#[2,] 1.227364 1.803509

## Now create a new object c1 with our special Wrapper for m1

#> c1<-makeCacheMatrix(m1)

## Check the value inside c1 to make sure the data is same
#> c1$getMatrix()
#         [,1]     [,2]
#[1,] 1.374692 2.029445
#[2,] 1.227364 1.803509

## Also check if we are getting NULL to make sure there is nothing cached for Inverted Matrix

#> c1$getInvertedCachedMatrix()
#NULL

## Finally call the cacheSolve by passing c1.
## Note there is no message begin written only the inverted matrix has been shown

#> cacheSolve(c1)
#          [,1]      [,2]
#[1,] -155.4898  174.9689
#[2,]  105.8173 -118.5192

## Invoke the wrapper function to check if the output is same as the cached object.
## Previously the same call yeilded NULL

#> c1$getInvertedCachedMatrix()
#          [,1]      [,2]
#[1,] -155.4898  174.9689
#[2,]  105.8173 -118.5192

## Call again cacheSolve to make sure this is indeed cached as you can see the "getting cached matrix" message.

#> cacheSolve(c1)
#getting cached matrix
#          [,1]      [,2]
#[1,] -155.4898  174.9689
#[2,]  105.8173 -118.5192

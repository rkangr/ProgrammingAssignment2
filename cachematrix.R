## The funcitons makeCacheMatrix and cacheSolve implement an extension to the matrix object
## which, in additions to the matrix itself contains the inverse of a matrix after the first time the inverse of the matrix
## has been requested. Later requests for the inverse of the matrix by cacheSolve command use pre-computed (cached) inverse 

## The functin makeCacheMatrix implements the object of cacheable matrix by using R lexical scoping rules
## calling this function creates an environment that contains (pointers to) variables M and Minv and returns four
## functions that can access this environment
## Each time makeCacheMatrix is used, a new environment is created, so many matrices with cacheable inverses can
## be used at the same time

makeCacheMatrix <- function(M = matrix()) {
  # Creates a matrix object with cacheable inverse.
  #
  # Args:
  #   M: A square matrix, assumed to be invertible (if given)
  #
  # Returns:
  #   a list of functions for setting and getting values of the matrix and it's inverse
  Minv <- NULL
  set <- function(W) {
    M <<- W # stores the value of the matrix W in variable M of the environment created at the time of using makeCacheMatrix function
    Minv <<- NULL # if a new matrix is defined, it's inverse is not known 
  }
  get <- function() M
  setInverse <- function(W) Minv<<- W #stores a matrix in the variable Minv of the environment. Should be used with the inverse of M
  getInverse <- function() Minv 
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The function cacheSolve returns a matrix that is the inverse of the matrix stored in
## the object 'x' created by makeCacheInverse function
## it uses the precomputed inverse, if possible
## if the precomputed inverse is not available, the functio computes it and in addition to
## returning the result it stores the inverse in the object 'x' for later use

cacheSolve <- function(x, ...) {
  # Returns a matrix that is the inverse of 'x' by using a precomputed inverse, if possible
  # Args:
  #   x: a list of functions created by makeCacheMatrix function
  # Returns:
  #   The inverse of the matrix that was used in calling x=makeCacheMatrix(M) or was set later by
  #   x$set function 
  Minv<- x$getInverse()
  if(!is.null(Minv)) { #has the inverse been computed already?
    ##message("getting cached data")
    return(Minv)
  }
  #the precomputed inverse was not available
  M <- x$get() #get the matrix that is stored in object 'x' (or more precisely, in the definition environment of the functions in 'x' )
  # as it is said in the assignment we assume that M is an invertible square matrix
  # otherwise some error checking should be done here
  Minv <- solve(M) #find the inverse of M 
  x$setInverse(Minv) #store the inverse for later use
  Minv
}

##sample code for trying out the functions
#N=1000
#A=matrix(runif(N*N),nrow=N)+N*diag(N) #a large invertible matrix
#x=makeCacheMatrix(A)
#b1=rnorm(N)
#sol1=cacheSolve(x)%*%b1 #the solution to the equation A z=b1, the inverse is computed
#head(sol1)
#b2=rnorm(N)
#sol2=cacheSolve(x)%*%b2 #the solution to the equation A z=b2, the cached inverse is used
#head(sol2)

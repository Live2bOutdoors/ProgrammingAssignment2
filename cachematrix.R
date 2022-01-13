## Two functions to reduce redundant computation of inverse of a matrix

## Function to create a list of functions to set the value of the matrix, get the value of the matrix, 
## set the value of the Inverse of matrix, and get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inverseX <- NULL
  set <- function(y) { #create set function so we can re-assign a new matrix without re-running makeCacheMatrix function
    x <<- y ## cache new matrix
    inverseX <<- NULL ## clear cache of existing inverse matrix when new matrix is defined
  }
  get <- function() x
  setsolve <- function(solve) inverseX <<- solve ## cache inverse of matrix
  getsolve <- function() inverseX
  list(set = set, get = get,   ##return list of functions to parent environment
       setsolve = setsolve,
       getsolve = getsolve)
}


## Function to check whether there is already a value stored for inverseX, if so, retrieve. If not, calculate it. Return inverse of matrix.

cacheSolve <- function(makeMatrix.object, ...) {
  inverseX.local <- makeMatrix.object$getsolve() #call getsolve() function to see if inverse of matrix is cached already
  if(!is.null(inverseX.local)) { #if inverse is already calculated (cached), then print message and return the inverse that is cached
    message("getting cached data")
    return(inverseX.local)
  }
  data <- makeMatrix.object$get() #if inverseX is Null (inverse hasn't been calculated yet), then get matrix to invert
  inverseX.local.calculated <- solve(data, ...) #invert matrix
  makeMatrix.object$setsolve(inverseX.local.calculated) #call function that will cache inverse of matrix
  inverseX.local.calculated #return inverse of matrix
}

##Test cases
# mat1.data <- c(runif(9))
# mat1 <- matrix(c(runif(9)),nrow=3,ncol=3,byrow=TRUE)
# mat1
# 
# test<- makeCacheMatrix(mat1)
# cacheSolve(test)  
# cacheSolve(test) 

# m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
# 
# myMatrix_object <- makeCacheMatrix(m1)
# cacheSolve(myMatrix_object)
# cacheSolve(myMatrix_object)
# 
# n1 <- matrix(c(6,2,8,4), nrow = 2, ncol = 2)
# myMatrix_object$set(n1)
# cacheSolve(myMatrix_object)
# cacheSolve(myMatrix_object)
# 
# n2 <- matrix(c(5/8, -1/8, -7/8, 3/8), nrow = 2, ncol = 2)
# myMatrix_object$set(n2)
# cacheSolve(myMatrix_object)
# cacheSolve(myMatrix_object)
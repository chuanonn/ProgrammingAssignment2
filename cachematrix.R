#Functions below are used to stores a matrix and caches the calculated inverse matrix.

#Assumption: 
#1.Matrix supplied is invertible.
#2.Matrix supplied is square matrix.

#Construct list containing functions to
#1.Set the value of the matrix
#2.Get the value of the matrix
#3.Set the value of the inverse matrix
#4.Get the value of the inverse matrix

makeCacheMatrix <- function(x) {
  m <- NULL
  
  set <- function(y) {  
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  
  list(set = set, 
       get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

#Returns the inverse matrix of supplied matrix.
#Calculates using solve() function if the inverse is not calculated.
#Or return the inverse matrix from cache if its already been calculated.

cacheSolve <- function(x, ...) {
  
  m <- x$getsolve()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

#Testing using mock data.
#x<-matrix(c(1,2,1,3),nrow=2,ncol=2);x

# [,1] [,2]
# [1,]    1    1
# [2,]    2    3

#y<-makeCacheMatrix(x)

#cacheSolve(y)

# [,1] [,2]
# [1,]    3   -1
# [2,]   -2    1

#cacheSolve(y)

#getting cached data
# [,1] [,2]
# [1,]    3   -1
# [2,]   -2    1

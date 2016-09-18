## Put comments here that give an overall description of what your
#the function put the inverse of a matrix into a cache state 
#in the parent environment using <<-, and then checks.  

## This is mostly the same as the example, except the mean reference is changed to matrix
#the 'mean' function is changed to 'solve'

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y=matrix()) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## Check the cache for the stored solution.  
#if it is null, solve.  if it is there,  return it.

cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
        ## Return a matrix that is the inverse of 'x'
}

m1<-matrix(1:4,2,2)
makeCacheMatrix(m1)
thing<- makeCacheMatrix(m1)
cacheSolve(thing)

m2<-matrix(c(1,2,3,5),2,2)
thing<- makeCacheMatrix(m2)
cacheSolve(thing)

m3<-matrix(c(1,1,1,1),2,2) #doesn't work.  Singular
thing<- makeCacheMatrix(m3)
cacheSolve(thing)

m4<-matrix(c(1,2,3,5,6,7,9,8,107),3,3)
thing<- makeCacheMatrix(m4)
cacheSolve(thing)

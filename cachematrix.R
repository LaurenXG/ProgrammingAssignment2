## I've made two functions, the first one creates a "matrix", which is actually a list
#containing the function to set and get the value of the matrix and set and get the invert
#matrix, storing that invert matrix in the cache. The second function checks if the matrix was inverted
#and stored in the cahce, if it was, it shows this inverted matrix along with a message. If it was not 
#stored in the cache (because it is a new matrix), it will invert the new matrix, store it in the cahe, 
#and return it without showing the message "getting cached data".

#We create a matrix, which is really a list containing a function to set the value 
#of the matrix, get the value of the matrix, set the value of the invert matrix, 
#and get the value of the invert matrix. Then, the function, cache this last invert matrix.


makeCacheMatrix <- function(x = matrix()) {
      i<- NULL
      set <- function (y){
            x <<- y
            i <<- NULL
      }
      get <- function () x
      setsolve <- function (solve) i <<- solve
      getsolve <- function () i
      list (set = set, get = get,
            setsolve = setsolve, 
            getsolve = getsolve)
}


## This function shows the inverse marix returned by the "makeCacheMatrix" function if the inverse matrix 
#was stored in the cahce, "cacheSolve" will return it from the cache and show the message "Getting cached data".
#If the matrix was changed (we introduce  new marix), the function will compute the "new" inverse of the new 
#matrix and store it in the cache.

cacheSolve <- function(x, ...) {
      i <- x$getsolve()
      if(!is.null (i)) {
            message ("getting cached data")
            return (i)
      }
      inversed <- x$get()
      i <- solve (inversed, ...)
      x$setsolve (i)
      i
      ## Return a matrix that is the inverse of 'x'
}

m1 <- matrix( data = c(1, 2, 3, 4),ncol= 2, nrow=2)
myMatrix<- makeCacheMatrix (m1)

cacheSolve (myMatrix)

n1 <- matrix( data = c(6, 2, 9, 7),ncol= 2, nrow=2)
myMatrix2 <- makeCacheMatrix(n1)

cacheSolve (myMatrix2)

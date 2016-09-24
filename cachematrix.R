## minh thien nguyen
## 09/24/2016
## makeCacheMatrix : this function creates a special "matrix" object that can cache its inverse
## cacheSolve: this function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
            ## If the inverse has already been calculated (and the matrix has not changed), 
            ## then cacheSolve should retrieve the inverse from the cache.    
            ## Write a short comment describing this function

## create a specail list that cache the inverse matrix
## and 4 nested functions set,get,setInverseMatrix,getInverseMatrix to do that
makeCacheMatrix <- function(cacheMatrix = matrix()) {
      cacheInverseMatrix <- NULL
      set <- function(y) {
            cacheMatrix <<- y
            cacheInverseMatrix <<- NULL
      }
      get <- function() cacheMatrix
      setInverseMatrix <- function(inverseMatrix) cacheInverseMatrix <<- inverseMatrix
      getInverseMatrix <- function() cacheInverseMatrix
      list(set = set,get = get, setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)
}


## check if the inverseMatrix = null then calculate and rutrn the inverse matrix
## other wise return inverse matrix from cache
cacheSolve <- function(matrix, ...) {
       inverseMatrix <- matrix$getInverseMatrix()
       if(!is.null(inverseMatrix) ){
             message("getting chached data")
       } else {
             inverseMatrix <- solve(matrix$get())
             matrix$setInverseMatrix(inverseMatrix)
       }      
       inverseMatrix
}

## test code 
A <- matrix(c(4,3,3,2),nrow = 2);
B <- matrix(c(4,1,3,1),nrow = 2);
specialMatrix <- makeCacheMatrix();
specialMatrix$set(A);
specialMatrix$set(B);
cacheSolve(specialMatrix)



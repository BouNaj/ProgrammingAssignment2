## The function computes the inverse of a matrix 
## and caches it

## This function caches the inverse of a matrix 

makeCacheMatrix <- function(x = matrix()){
        matinv <- NULL
          set <- function(y){
          x <<- y
          matinv <<- NULL
  }
        get <- function() {x}
        setinverse <- function(inverse) {matinv <<- inverse}
        getinverse <- function() {matinv}
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of a matrix 
cacheSolve <- function(x, ...){
        inverse <- x$getinverse()
        if(!is.null(inverse)){
            message("getting chached data")
            return(inverse)
  }
        matr <- x$get()
## Return a matrix that is the inverse of 'x'
        matinv <- solve(matr, ...)
        x$setinverse(matinv)
        matinv
}

## Testing the functions
## Create a matrix using sample(). 
mat.test <- makeCacheMatrix(matrix(sample(9), ncol = 3))
## Get the matrix 
mat.test$get()
## Get the inverse of mat.test.
mat.test$getinverse()
## If null, then inverse is not cashed. 
## Use cacheSOlve to compute inverse of mat.test
cacheSolve(mat.test)
## The inverse computed, I run settinverse again 
mat.test$getinverse()

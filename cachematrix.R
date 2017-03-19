## Example usage
## source("cachematrix.R")
## a$set(matrix(4:7, 2, 2)) 
## cacheSolve(a) 
##       [,1] [,2]
## [1,] -3.5    3
## [2,]  2.5   -2
## cacheSolve(a) 
## getting cached data
##      [,1] [,2]
## [1,] -3.5    3
## [2,]  2.5   -2

## Matrix inversion is a costly computation and
## there may be some benefit to caching the inverse of a 
## matrix rather than compute it repeatedly.

## This function creates a special "matrix" object 
## that can cache its inverse.
## This function will 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        ##define a cache matrix and initialize to null
        m <- NULL
        set <- function(y) {
                ## assigning input matrix to x
                x <<- y
                ## re-initialise to null
                m <<- NULL
        }
        ## return martix x
        get <- function() x
        
        ##setting m to inverse of x
        setinverse <- function(inverse) m <<- inverse
        
        ##getting cached inverse of x
        getinverse <- function() m
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated 
## (and the matrix has not changed), then the
## cachesolve should retrieve the inverse from the cache.
## Assumption - Matrix Supplied is always invertible

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        
        ##solve -Inverse of a square matrix	
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

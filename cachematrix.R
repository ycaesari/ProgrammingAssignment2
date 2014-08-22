## The following two functions enable us to cache the result of computing the inverse of a matrix.
## The porpuse is to save the time it takes to calculate the inverse, and if it was done already, 
## use the computed result.

## In order to use these function, one should create a cacheMatrix object in the following way. 
## (mat is the the matrix we want to calculate the inverse on).
## cacheMatrix <- makeCacheMatrix(mat)

## Then, in order to get the inverse value and cache the result, use
## inverseOfMatrix <- cacheSolve(cacheMatrix)

## subsequent calls to cacheSolve will return the cached value (unless the value is reseted by get(), 
## see next).

## Example:

## > mat <- matrix(c(4, 2, 7, 6), nrow=2, ncol=2)
## > mat
##      [,1] [,2]
## [1,]    4    7
## [2,]    2    6
##
## > cacheMat <- makeCacheMatrix(mat)
## > inverseOfMat <- cacheSolve(cacheMat)
## > inverseOfMat
##      [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4
## 
## > inverseOfMat <- cacheSolve(cacheMat)
## getting cached data
##

## The makeCacheMatrix() stores the matrix and the inverse in the enviroment of the cachedMatrix object.
## setinverse() and getinverse() are used to store and retrieve the inverse, respectively.
## get() is used to get the matrix and if set() is called, the matrix is updated with a new one and the 
## inverse is nullified so it will be calculated again for the modified matrix.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function solves the matrix and return inverse of the matrix which is in the CacheMatrix object.
## The function expects to recieve a CacheMatrix object created by the makeCacheMatrix. it then checks if 
## the inverse has already been calculted. If yes, the cached value is returned. Otherwise, the matrix
## in the CacheMatrix object is fetched, the inverse is calculated and the result is set in CacheMatric 
## for future use. The calculated inverse is then returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i  
}

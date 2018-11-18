## programmingAssignments2
## Coursera week 3 Assignemnt 2
## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly 
## The following two functions will cache the inverse of a matrix
## Write the following functions:
## makeCacheMatrix: 
## The first function, Cachematrix creates a special "vector", which is really a list containing a function to
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of inverse of the matrix
## 4.get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()){
    Inver <- NULL
    set <- function(y) {
        x <<-y
        Inver <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) Inver <<- inverse
    getinverse <- function() Inver 
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve: 
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.

cacheSolve <- function (x, ...){
    inver <- x$getinverse()
    if(!is.null(inver)) {
        message("getting cached data")
        return(inver)
    }
    data <- x$get()
    inver <- solve(data)
    x$setinverse(inver)
    inver
}

## Sample results
x <- matrix(c(4,3,2,1),2,2)
XM<- makeCacheMatrix(x)
cacheSolve(XM)
> cacheSolve(XM)
     [,1] [,2]
[1,] -0.5    1
[2,]  1.5   -2
> XM$get()
     [,1] [,2]
[1,]    4    2
[2,]    3    1

## P2P Assignment 2
## Cached inversed matrix

##Testing scenario:
## 1. Creating test 2x2 matrix x
##> x=matrix(c(1,2,3,4), nrow = 2, ncol = 2)
##> x
##[,1] [,2]
##[1,]    1    3
##[2,]    2    4

## 2. Calculating inverse for test matrix manualy
##> solve(x)
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

## 3. Initializing cachable matrix x1 based on x
##> x1=makeCacheMatrix(x)
##> x1$get()                <---- OK?
##[,1] [,2]
##[1,]    1    3
##[2,]    2    4

## 4. Performing cacheSolve twice
##> cacheSolve(x1)
##[,1] [,2]                 <---- OK?
##[1,]   -2  1.5
##[2,]    1 -0.5
##> cacheSolve(x1)
##getting cached data       <---- OK?
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

## 5. Changing matrix in x1
##> xx=matrix(c(4,3,2,1), nrow = 2, ncol = 2)
##> solve(xx)
##[,1] [,2]
##[1,] -0.5    1
##[2,]  1.5   -2
##> x1$set(xx)              <---- changing matrix here
##> x1$get()                <---- OK?
##[,1] [,2]
##[1,]    4    2
##[2,]    3    1

## 6. Solve cachable matrix twice
##> cacheSolve(x1)          <---- OK (No cache for new matrix)?
##[,1] [,2]
##[1,] -0.5    1
##[2,]  1.5   -2
##> cacheSolve(x1)          <---- OK (New matrix cached)?
##getting cached data
##[,1] [,2]
##[1,] -0.5    1
##[2,]  1.5   -2

## 7. Verifying that matrix solved correctly
## OK. Result correspond to manual solve

## P.S. Feel free to use this scenario for grading your peers :)

## Define cachable matrix
makeCacheMatrix <- function(x = matrix()) {
    inv_x <- NULL
    set <- function(y) {
        x <<- y
        inv_x <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv_x <<- solve
    getinv <- function() inv_x
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Solve matrix with caching result
cacheSolve <- function(x, ...) {
    inv_x <- x$getinv()
    if(!is.null(inv_x)) {
        message("getting cached data")
        return(inv_x)
    }
    data <- x$get()
    inv_x <- solve(data, ...)
    x$setinv(inv_x)

    ## Return a matrix that is the inverse of 'x'
    inv_x
}

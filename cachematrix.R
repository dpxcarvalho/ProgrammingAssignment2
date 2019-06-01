## R Programming Course - Week 3 Assignment
## Student Daniel Peixoto de Carvalho
## Created in 29 May 2019

## The function makeCacheMatrix creates a special "matrix" object 
## that can cache its inverse

makeCacheMatrix <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## The function cachesolve computes the inverse of a matrix returned
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cacheSolve function should
## retrieve the inverse from the cache

cachesolve <- function(x, ...) {
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

# Sample evaluation of the inverse of a matrix
# > a<-matrix(c(4,2,7,6),2,2)
# > a
# [,1] [,2]
# [1,]    4    7
# [2,]    2    6
# > solve(a)
# [,1] [,2]
# [1,]  0.6 -0.7
# [2,] -0.2  0.4

# Sample evaluation of the week 3 assignment
# > a <- matrix(c(4,2,7,6),2,2)
# > a
# [,1] [,2]
# [1,]    4    7
# [2,]    2    6
# > b <- makeCacheMatrix(a)
# > c <- cachesolve(b)
# > c
# [,1] [,2]
# [1,]  0.6 -0.7
# [2,] -0.2  0.4

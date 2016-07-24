## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
## cacheSolve: This function computes the inverse of the special "matrix" returned by 
##             makeCacheMatrix above. If the inverse has already been calculated (and 
##             the matrix has not changed), then the cachesolve should retrieve the 
##             inverse from the cache.

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    minvr <- NULL #set initial cached inverse value to NULL
    set <- function(y) {
        x <<- y # assign a value to an object in called environment
        minvr <<- NULL # everytime a new matrix is assigned null the cached inverse value
    }
    setInvr <- function(minv) {
        minvr <<- minv
    }
    getInvr <- function() minvr
    get <- function() x
    list(set = set, get = get, setInvr = setInvr, getInvr = getInvr)
}


## Caches the inverst of a matrix and returns it instead

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invr <- x$getInvr()
    if(!is.null(invr)) {
        print("Returning inverse from cache")
        return(invr)
    }
    invr <- solve(x$get())
    x$setInvr(invr)
    invr
}

## Sample results

# source("cachematrix.R")

# mtr <- makeCacheMatrix(matrix(1:4,2,2))

# cacheSolve(mtr)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

# cacheSolve(mtr)
# [1] "Returning inverse from cache"
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

# > mtr$set(rbind(c(1,2,3),c(0,1,4),c(5,6,0)))

# > mtr$get()
# [,1] [,2] [,3]
# [1,]    1    2    3
# [2,]    0    1    4
# [3,]    5    6    0

# > cacheSolve(mtr)
# [,1] [,2] [,3]
# [1,]  -24   18    5
# [2,]   20  -15   -4
# [3,]   -5    4    1

# > cacheSolve(mtr)
# [1] "Returning inverse from cache"
# [,1] [,2] [,3]
# [1,]  -24   18    5
# [2,]   20  -15   -4
# [3,]   -5    4    1
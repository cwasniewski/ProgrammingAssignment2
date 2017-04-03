## Put comments here that give an overall description of what your
## functions do

## Built makeCacheMatrix in parallel with the example function provided
## returns a list of functions to set the matrix values, retrieve those values,
## set the inverse of the matrix, and retrieve the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(solve) inv <<- solve
    getInv <- function() inv
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Built cacheSolve first based off of example used for mean
## Will tweak it as needed after makeCacheMatrix is completed
## and any major surprises that aren't yet noted become clear

## There were no surprises

## cacheSolve checks whether or not the inverse of a matrix has already been
## calculated, then if not - calculates the inverse and caches that inverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInv(inv)
    inv
}

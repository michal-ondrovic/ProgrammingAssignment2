
## makeChacheMatrix creates a matrix, which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inverseMatrix <- NULL
        set <- function(y) {
                x <<- y
                inverseMatrix <<- NULL
        }
        get <- function() x
        setInverseMatrix <- function(solve) inverseMatrix <<- solve
        getInverseMatrix <- function() inverseMatrix
        list(set = set, get = get,
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)
}


## chacheSolve calculates an inverse matrix of matrix created by makeCacheMatrix function
## It first checks to see if the inverse matrix has already been calculated.
## If so, it gets the inverse matrix from the cache and skips the computation.
## Otherwise, it calculates the inverse matrix of the mtrx
## and sets the value of the inverse matrix in the cache via the setInverseMatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseMatrix <- x$getInverseMatrix()
        if(!is.null(inverseMatrix)) {
                message("getting cached data")
                return(inverseMatrix)
        }
        mtrx <- x$get()
        inverseMatrix <-solve(mtrx, ...)
        x$setInverseMatrix(inverseMatrix)
        inverseMatrix
}

# This R script calculates matrix inverse.  To to reduce the costly
# re-computation for a large matrix, it utilizes a simple caching
# utility to return the cached result if the identical matrix inverse is
# requested.

makeCacheMatrix <- function(x = matrix()) {
    # A simple cache utility for matrix inverse.
    #
    # Args:
    #   x: the input matrix
    #
    # Four sub-functions are defined.
    #   get():      Return the matrix x.
    #   set(y):     Assign x with the new matrix y.
    #   getInv():   Return inverse of matrix x.
    #   setInv(y):  Set the inverse of matrix x.  The
    #               value 'y' was provided by the caller.
    #
    # Calling the defined sub-functions:
    #   A sub-function list is created so that the sub-functions
    #   can be invoked via $get(), $set(y), etc.

    # The inverse matrix is set to NULL upon creation of a new object for
    # matrix 'x'.  It is required so that an immediate getInv() call will
    # return NULL.
    invMatrix <- NULL

    # Get the cached matrix.
    get <- function() x

    # Set the cached matrix 'x' with new value 'y'.
    set <- function(y) {
        x <<- y
        invMatrix <<- NULL
    }

    # Get the inverse of the matrix 'x'.
    getInv <-function() invMatrix

    # Set value of the inverse of matrix 'x'.
    setInv <- function(y) invMatrix <<- y

    # Exporting the get, set, getInv and setInv sub-functions so that the
    # sub-functions can be called via $get(), $set(y), etc.
    list(get = get, set = set, getInv = getInv, setInv = setInv)
}

cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of matrix 'x'.
    #
    # Args:
    #   x: the input matrix
    #
    # Returns:
    #   inverse of matrix 'x'

    # If makeCacheMatrix object for 'x', named "f", does not exist OR
    # if the matrix 'x' is not exactly equal to the cached matrix in "f",
    # we initialize a new makeCacheMatrix object "f".

    # It is important that we save the new object "f" into the
    # parent environment with "<<-" operator so that the object "f" exists
    # when this function is called again.
    if (!exists("f") || !identical(f$get(), x)) {
        f <<- makeCacheMatrix(x)
    }

    # Is the inverse matrix previously cached?  If yes, returned the
    # cached inverse value.
    inverse <- f$getInv()
    if(!is.null(inverse)) {
        message("getting the cached inverse matrix")
        return(inverse)
    }

    # The matrix inverse is not cached.  Calculate the inverse and set the value.
    mat <- f$get()
    inverse <- solve(mat, ...)
    f$setInv(inverse)
    inverse
}

testCacheSolve <- function(n) {
    # Test case for the cacheSolve function.
    # Repeat invocation of testCacheSolve(n) for the same n will use the
    # cached result.
    # 
    # Usage examples:
    #   testCacheSolve(3)
    #   testCacheSolve(3)   ## You should see the "cached message".
  
    # Calculate the inverse matrix of diag(n) which should be diag(n).
    m <- diag(n)
    mInv <- cacheSolve(m)
    
    # We expect m %*% mInv to be a diagonal matrix diag(n).
    if (! identical(m, m %*% mInv)) {
        # Stop with error message.
        stop("Incorrect matrix inverse result !!")
    } else {
        message("The correct matrix inverse for")
        print(m)
        message("is")
        print(mInv)
    }
}
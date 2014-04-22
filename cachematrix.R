# Define a function which takes as input an (optional) matrix and returns a list of functions
# to operate on that matrix: set(), get(), setinverse(), getinverse().  The matrix and it's
# inverse will be cached in the environment of the function.

makeCacheMatrix <- function(cached.matrix = numeric()) {
    cached.inverse <- NULL
    set <- function(new.matrix) {
        # Following two assignments superassign in the environment of makeCacheMatrix(), not set().
        # Thus such assignments will outlive the running of set() and persist - i.e. be cached.
        cached.matrix <<- new.matrix                      # save the new matrix value
        cached.inverse <<- NULL                           # clear the cached inverse
    }
    get <- function() cached.matrix                       # just return cached matrix
    setinverse <- function(inv) cached.inverse <<- inv    # superassignment - same result as in set().
    getinverse <- function() cached.inverse               # just return cached inverse
    # Return the list of functions.  Note that neither the matrix or its inverse are explicitly returned.
    # Rather, they exist in the environment in which the functions in the list were defined.
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

# Define a function to operate on the list representing a cached matrix returned by makeCacheMatrix().
# Additional arguments represented by ... will be passed to function solve() when inverse is calculated.
# Usage is as follows.  First, create a cached matrix via makeCacheMatrix() which returns a list.  Let's
# call this list myMatrix for demonstration purposes.
#
#   To reset matrix value do:    myMatrix$set(new.value)
#   To get matrix value do:      myMatrix$get()
#   To get matrix inverse do:    cacheSolve(myMatrix, other.args.for.solve)
#
# Do not ever call myMatrix$setinverse() or myMatrix$getinverse()) directly.  All access to the
# matrix inverse should be via the cacheSolve() function.

cacheSolve <- function(x, ...) {
    # Extract cached inverse value - might be NULL.
    inv <- x$getinverse()
    # If cached inverse exists - return it without any computation required.
    if ( !is.null(inv) ) {
        message("getting cached inverse")
        return(inv)
    }
    # Cached inverse did not exist.
    data <- x$get()                # extract cached matrix
    inv <- solve(data, ...)        # calculate inverse anew
    x$setinverse(inv)              # cache the new inverse
    inv                            # return inverse to caller
}

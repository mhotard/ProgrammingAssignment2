## Put comments here that give an overall description of what your
## functions do

## this function creates a special matrix that is actually a list. 
## the list has four elements: 1) set the value of the matrix
## 2) get the value of the matrix, 3) set the value of the matrix's inverse
## 4) get the value of the matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL # sets a placeholder to null
    set <- function(y) { #sets the value of the matrix
        x <<- y    
        m <<- NULL
    }
    get <- function() x # gets the value of the matrix
    setinverse <- function(solve) m <<- solve # sets the inverse of the matrix
    getinverse <- function() m # gets the value of the 
    list(set = set, get = get, # creates the list that holds the values
         setinverse = setinverse,
         getinverse = getinverse)

}


## will calculate the inverse of a matrix, but first checks to 
## see if it's been calculated already. If so, then it pulls the
## value from the cache

cacheSolve <- function(x, ...) {
    
    m <- x$getinverse() 
    if(!is.null(m)) { ## checks to see if it exists, if so then it pullls from cahce
        message("getting cached data")
        return(m)
    }
    data <- x$get() $ #gets the value of the matrix
    m <- solve(data, ...) # calculates inverse of the matrix
    x$setinverse(m) # stores the calculated results in the variables
    m # is the inverse of the matrix
        ## Return a matrix that is the inverse of 'x'
}

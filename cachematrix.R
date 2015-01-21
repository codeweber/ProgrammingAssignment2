#Functions to calculate the inverse of a matrix, using caching
#If an inverse is cached this will be returned,
#otherwise an inverse will be calculated, cached and returned

#makeCacheMatrix
#this function is used to cache the matrix and its inverse
#returns a list containing functions to get and set values of 
#x and it's inverse
#note that x, inverse, set, get, setInverse and getInverse exist in
#an environment created when makeCacheMatrix is created
makeCacheMatrix <- function(x = matrix()) {

    #Set the local variable inverse to NULL
    #Note that this value is defined in the environment in which the 
    #this function is defined and is used to cache the inverse
    inverse <- NULL

    #set takes a matrix as input and stores this as x. 
    #the cached inverse is also reset
    #The input is assumed valid, i.e. invertible. No checks are performed
    set <- function(y) {
      x <<- y
      inverse <<- NULL    
    }

    #get returns the cached matrix  
    get <- function() {
        print(environment())
        print(objects())
        x
    }

    #setInverse is used to cache the value of the inverse
    setInverse <- function(i) {
      inverse <<- i
    }

    #getInverse returns the cached value of the inverse
    getInverse <- function() inverse

    #Return a list of getter and setter functions
    list(get = get, set = set, getInverse = getInverse, setInverse = setInverse)
}


#cacheSolve 
#returns the inverse of a matrix given an object x created by 
#makeCacheMatrix. If x contains a cached inverse, this is returned
#otherwise an inverse is calculated and this is cached in x
cacheSolve <- function(x, ...) {

    #get the inverse cached in x
    i <- x$getInverse()

    #if the inverse is not null, return it
    if ( !is.null(i) ) {
        message("getting cached inverse...")
        return(i)
    }

    #if inverse is null, calculate it using solve and cache it in x
    message("calculating inverse and caching...")
    m <- x$get()
    i <- solve(m)
    x$setInverse(i) 
    i
}

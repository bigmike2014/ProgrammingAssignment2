## The below functions are used to calculate a matrix inverse in an efficient manner - e.g. if you are in a loop.  The makeCacheMatrix function creates a special object that stores a matrix and caches its inverse.  

## This function sets up a list containing a function to: set the value of the matrix, get the value of the matrix, set the value of the matrix inverse and get the value of the matrix inverse

makeCacheMatrix <- function(X = matrix()) {
	
	## Set inverse Cache to null initially
	inverse <- NULL

	## set function to set the value of the matrix and clear 	the inverse

	set <- function(Y) {
			X <<- Y
			inverse <<- NULL
	}
	
	## get function returns the value of the matrix

	get <- function() X


	## set inverse function sets the value of the cached 	inverse matrix

	setinverse <- function(s) inverse <<- s

	## getinverse function returns the cached inverse matrix

	getinverse <- function() inverse

	list(set = set, get = get, 
	 	setinverse = setinverse, 
		getinverse = getinverse)

}


## This function looks to see if the inverse of the matrix is cached.  If it is it returns the matrix inverse, otherwise it calculates the matrix inverse and sets it to the cache.

cacheSolve <- function(X, ...) {
        ## Return a matrix that is the inverse of 'X'

	inverse <- X$getinverse()  ## get cached inverse
  	if(!is.null(inverse)) {

		## if cached inverse not null return cached inverse
 		## and return

     		message("getting cached data")  
       	return(inverse)
    	}
      
	## otherwise   	
	data <- X$get()
    	inverse <- solve(data, ...)
    	X$setinverse(inverse)
   	inverse

}



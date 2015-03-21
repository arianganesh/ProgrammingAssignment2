## Overall Description: 
##  makeCacheMatrix : Getter/Setter functions for matrix & its inverse
##  cacheSolve: This function returns inverse of matrix - by first checking from cache


makeCacheMatrix <- function(x = matrix()) {
		### Function Description : Accessor functions for Cache matrix and its inverse
		### Input Arguments: A Matrix
		### Return: List of matrix accessor functions & inverse accessor functions
		
		cached.inverse <- NULL
		
		# set the matrix cache & its inverse
		set <- function(y) {
				x <<- y
				cached.inverse <<- NULL
		}
		
		# returns the cached matrix
		get <- function() {
				return (x)
		}
		
		# Sets the calculated inverse into cache
		setinverse <- function(calculated.inverse){
				cached.inverse <<- calculated.inverse
		}
		
		#Retrieves the cached inverse.
		#cached.inverse would either hold NULL or the valid inverse 
		getinverse <- function() {
				return(cached.inverse)
		}
		
		# return : matrix & inverse accessor functions
		list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)

}


 
cacheSolve <- function(x, ...) {
		### Function Description : Returns the inverse of a matrix - 
		##    by first checking in cache, else calculates inverse & stores value in cache
		### Input Arguments: Input Matrix which is generated using makeCacheMatrix
		### Return: Matrix which is inverse of input
		
		
		# Getting the cached inverse 
		cached.inverse <- x$getinverse()
	
		# Checking the value retrieved in above step. 
		# If it was NOT-NULL, it meant the cache existed & 
		# hence returning the cached inverse 
		if(!is.null(cached.inverse)) {
				message("Getting cached inverse of matrix")
				return(cached.inverse)
		}
		
		#Getting matrix
		input.matrix <- x$get()
	
		# Calculating the inverse using solve function 
		inverse <- solve(input.matrix)
	
		# Setting the inverse into cache 
		x$setinverse(inverse)
		
		# Returning the inverse of matrix
		return(inverse)}

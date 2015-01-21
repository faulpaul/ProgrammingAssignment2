## Put comments here that give an overall description of what your
## functions do.
## Due to the fact, that Matrix inversion is a costly computation,
## there are some benefits in caching the inverse rather than 
## computing it again.
## The following functions are used to create and cache the inverse
## of a matrix.


## Write a short comment describing this function
## makeCacheMatrix creates a special matrix which is in fact a list,
## containing a function to do four things:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
	## create the matrix
	i <- NULL
	## set the value of the matrix
	set <- function(y){
		x <<- y
		i <<- NULL
	}
	get <- function() x
	## set the value of the inverse
	setinverse <- function(inverse) i <<- inverse
	## get the value of the inverse 
	getinverse <- function() i
	## create a list containing the functions
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
## This function creates and returns the actual inverse of the
## matrix. Before calculating the inverse, the function checks if there
## already exists a cached version of the inverse. In which case it 
## returns the cached value an does not compute it again. If there is no
## cached value, the function computes the inverse and writes the value
## into the cache. 
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	## asign x_inv the value from the cache. If the inverse exists
	## the variable will not be NULL an the function will return the
	## cached inverse
	i <- x$getinverse()
	if (!is.null(i)){
		message("getting cached data")
		return(i)
	}
	## if the value was not cached, the function will compute
	## the inverse ...
	data <- x$get()
	i <- solve(data, ...)
	## ... write it in the cache ...
	x$setinverse(i)
	## ... and return the value.
	i
}

## some testing cases
## the first run will compute the inverse
## in the secound run, the value should be taken from the cache
## the examples have been taken from the german wikipedia article
## http://de.wikipedia.org/wiki/Inverse_Matrix
## so we can make sure there is an actual inverse

## test A, run 1
##x <- rbind(c(2, 5), c(1, 3))
##i <- makeCacheMatrix(x)
##i$get()
##cacheSolve(i)

## test A, run 2
##cacheSolve(i)

## test B, run 1
##x <- rbind(c(1, 2, 0), c(2, 4, 1), c(2, 1, 0))
##i <- makeCacheMatrix(x)
##i$get()
##cacheSolve(i)

## test B, run 2
##cacheSolve(i)

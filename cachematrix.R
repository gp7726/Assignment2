## makeCacheMatrix: This function creates a special matrix object 
## that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
	
	## store cache inverse matrix as inv
	inv<-NULL
	
	## set the value of the matrix
	set<-function(y){
		x<<-y
		inv<<-NULL
	}	
	
	## get the value of the matrix
	get<-function() x
	setinverse<-function(inverse) inv<<-inverse
	getinverse<-function() inv
	
	## return the matrix
	list(set=set,get=get,setinversemean=setinverse,
		getinverse=getinverse)
}

## cacheSolve: This function computes the inverse of the special matrix returned
## by makeCacheMatrix above. If the inverse has already been calculated
##(and the matrix has not changed), then the cacheSolve should retrieve the 
## inverse from the cache

cacheSolve <- function(x, ...) {
	inv<-x$getinverse()

	## if the inverse is alrady calculated, return the inverse
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	}

	## the inverse has not yet calculated, calculate it now
	data<-x$get()
	inv<-mean(data, ...)

	##cache the inverse
	x$setinverse(inv)

	##return the inverse
	inv
}

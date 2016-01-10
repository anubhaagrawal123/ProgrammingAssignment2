##Functions to cache inverse of a matrix


## Creates a matrix object to cache it's inverse
makeCacheMatrix <- function(y = matrix()) {
	k<-NULL
	set<-function(matrix){
		y<<-matrix
		k<<-NULL
	}
	get<-function(){
		y
	}
	setInverse<-function(inv){
		k<<-inv
	}
	getInverse<-function(){
		k
	}
	list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)

}


## Compute inverse of matrix returned by "makeCacheMatrix". 
## If inverse has already been calculated then retrieve inverse from cache.


cacheSolve <- function(x, ...) {
	y<-x$getInverse()
	if(!is.null(y)){
		message("Cached Data")
		return(y)
	}
	d<-x$get()
	y<-solve(d)%*% d
	x<-setInverse(y)
	y
}
}

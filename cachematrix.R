## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). Funcitons below will cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {	#set x to store matrix
		im <- NULL							#set im to store inverse matrix
		set <- function(y) {				#define set fun to receive new matrix
			x <<- y							#new matrix stored in parent env, in x
			im <<- NULL						#new matrix has inverse null, set in parent env
		}
		get <- function() x					#get fun will get matrix x from makeCacheMatrix env
											#..it doesn't take any value (i think)????
		setinverse <- function(z) im <<- z	#fun will receive var z and store it in im 
		getinverse <- function() im			#fun will simply get im from this env
											#..it doesn't take any value (i think)????
		list(set = set, get = get,			#will display list of 4 fun created here
			 setmean=setmean,				#they all have names with which we can call
			 getmean=getmean)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(a, ...) {			# will take a as arguement. 
											# this a will be same where output of makeCacheMatrix
											# is stored, which is a list of 4 functions.
											# so prior to running this example, user will have to
											# run a <- makeCacheMatrix and use a as input to 
											# function cacheSolve
											# why ... is also taken as argument(input)????
		im <- a$getmean()					# im is captured using getmean fun
		if(!is.null(im)) {					# if im has value, its displayed 
			message("getting cache data")	
			return(im)
		} 
											# im is null only then following will be executed
		data <- a$get()						# x in above function is read into data
		im <- solve(data)					# inverse of x is calculated
		a$setinverse(im)					# inverse is stored in im by calling setinverse fun
		im									# also, im will be displayed
}
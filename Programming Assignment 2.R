## Put comments here that give an overall description of what your
## functions d o
makeCacheMatrix <- function(x = matrix()) {
        m<-NULL #here is stored the inverted matrix
        set<-function(y){
                x<<-y
                m<<-NULL #innitial value is null
        }
        get<-function() x #provides the input data
        setmatrix<-function(solve) m<<- solve #sets the inverted matrix
        getmatrix<-function() m  #returns the inverted matrix
        list(set=set, get=get,
             setmatrix=setmatrix, #sets inverted matrix
             getmatrix=getmatrix) # gets inverted matrix
}

cacheSolve <- function(x=matrix(), ...) {
        m<-x$getmatrix() #this gets the inverted matrix from the object x
        if(!is.null(m)){
                message("getting cached data")
                return(m) #returns the inversion of the matrix
        }
        matrix<-x$get() #if the inversion is not yet calculated it takes the data of the original matrix
        m<-solve(matrix, ...) # and here it inverts the matrix 
        x$setmatrix(m) #and sets it to the object
        m #returns solved results
}

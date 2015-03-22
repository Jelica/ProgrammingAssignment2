## Functions that cache the inverse of a matrix

## Creates a special matrix object that can cahe its inverse

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        set_inv_matrix<-function(solve) m<<- solve
        get_inv_matrix<-function() m
        list(set=set, get=get,
             set_inv_matrix=set_inv_matrix,
             get_inv_matrix=get_inv_matrix)
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x=matrix(), ...) {
        m<-x$get_inv_matrix()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        matrix<-x$get()
        m<-solve(matrix, ...)
        x$set_inv_matrix(m)
        m
}

## A pair of functions that caches a matrix and its inverse matrix
## 

## makeCacheMatrix function that creates list that contains matrix and its inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setmatrix<-function(solve) m<<- solve
        getmatrix<-function() m
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}


## cacheSolve solves an inverse matrix if the inverse matrix was not previously calculated

cacheSolve <- function(x, ...) {
        m<-x$getmatrix()
        if(!is.null(m)){
                message("getting cached data")
                return(m)  ## Return a matrix that is the inverse of 'x'
        }
        matrix<-x$get
        m<-solve(matrix, ...)
        x$setmatrix(m)
        m
}

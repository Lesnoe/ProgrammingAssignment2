##  makeCacheMatrix  create inverse matrix for incomming matrix'x' 
## the treatment  results  is store in function "getinv"
## also function returnd list



makeCacheMatrix <- function(x = matrix()) {
        s<-NULL
        set<-function(y){
                x  <<- y
                s  <<-NULL
        }
        get <- function()x
        setinv<- function(solve) s<<- solve
        getinv<- function() s
        list(set=set,get=get,setinv=setinv,getinv=getinv)
}




## Below function get used abofe function and return
## the inverse matrix 'x' from cache(if there is)  or
## put new value in cache

cacheSolve <- function(x, ...) {
        s <- x$getinv()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data,...)
        x$setinv(s)
        s
}          


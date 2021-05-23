## Writing a pair of functions that cache the inverse of a matrix

##A function to create a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()){

        ##initializing the inverse object
        i<-NULL

        ##Method to set the matrix
        set<-function(y){
                x<<-y
                i<<-NULL
        }
        
        ##Method to get the matrix
        get<-function(){
                x
        }
        
        ##Method to set the inverse of the matrix
        setInverse<-function(inverse){
                i<<-inverse
        }
        
        ##Method to get the inverse of the matrix
        getInverse<-function(){
                i
        }
        
        ##Returning a list of all the methods.
        ##Each element in the list is named.
        ##This allows us to use the $ form of the extract operator
        ##To access the functions by name 
        list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}

##A function to compute the inverse of the matrix created in makeCacheMatrix
##if inverse already calculated and the matrix has not changed, 
##Then the cachesolve retrieves the inverse from the cache.

cacheSolve<- function(x, ...){

        ##To retrieve inverse of matrix x
        i<-x$getInverse()

        ##check to see whether the i is null
        ##if not null, then return the cached inverse 
        if (!is.null(i)) {
                message("Getting cached data")
                return(i)
        }

        ##To get matrix from the input object
        data<-x$get()

        ##Calculate the inverse of matrix
        i<-solve(data)

        ##Set the inverse to the input object
        x$setInverse(i)

        ##Return the inverse matrix
        i
}
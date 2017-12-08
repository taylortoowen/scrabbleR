source("../Session 7/Stack.R")


`%C%` <- function(n, k){
    stopifnot(n >= 0)
    facTable <- numeric(n+1)
    facTable[1] <- 1
    for( i in 1:(n+1)){
        facTable[i+1] <- i * facTable[i]
    }
    #print(facTable)
    result <- facTable[n+1]/(facTable[k+1]*facTable[n+1-k])
    return(result)
}

intersetingExample <- c("a", "b", "c", "d")



chooseTest_1 <- function(){
    expected <- 210
    actual <- 10 %C% 4
    stopifnot( expected == actual )
    print("10 %C% 4 passed ")
}

chooseTest_2 <- function(){
    expected <- 1
    actual <- 10 %C% 10
    stopifnot( expected == actual )
    print("10 %C% 10 passed ")
}


# Thinking out loud

# bottom up approach similar to how we did grow

# when we have a set of 1 we will return that set and the empty set
    # side note.. do we need the empty set?

# when we have a set of X, we will still return that set and the empty set

# example with myVec <- c("a")
    # myVec[seq_along(myVec)] --> c("a")
    # AKA myVec[1:1] --> c("a")
    # myVec[-seq_along(myVec)] --> character(0)



includeExclude <- function(vec, target, size){
    list( 
        include = target[vec], 
        exclude = target[-vec], 
        nextSize =  size+1
        )
}


partitionSet <- function(vec){
    len <- length(vec)
    # set up results table
    results <- vector("list", (2**len)-1)
    results[[(2**len)-1]] <- vec[ seq_along(vec) ] # full set
    i <- 1
    # trivial case
    if( len == 1 ) return( results )
    # set up the stack
    myStack <- Stack()
    seed <- includeExclude(1, 1:2, 2)
    myStack$push( seed )
    # seen table for new nums
    seen <- logical(len)
    # begin while loop
    while( !myStack$isEmpty() ){
        top <- myStack$pop()
        if( top$nextSize > len ){
            # this means it's ready
            results[[i]] <- vec[top$include]; i <- i + 1 # result pair can be written
            results[[i]] <- vec[top$exclude]; i <- i + 1
        }
        else{
            # keep going
            newRange <- 1:top$nextSize
            incl <- includeExclude(top$include, newRange, top$nextSize)
            excl <- includeExclude(top$exclude, newRange, top$nextSize)
            myStack$push(incl)
            myStack$push(excl)
            if( !seen[top$nextSize] ){
                seen[top$nextSize] <- TRUE # we've seen the new number, mark it as seen
                new <- includeExclude(top$nextSize, newRange, top$nextSize) 
                myStack$push(new) # add it to the mix
            }
        }
    }
    return( results )
}
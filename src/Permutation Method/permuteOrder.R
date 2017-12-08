source("../Session 7/Stack.R")
# see growHint.R for super literal, line-by-line, demonstrations 
# of what will be occuring in your grow function

# grow is just one part of the dynamic programming solution I am thinking about
# for how to permute order. If you know how to permute the order of all vectors of length N-1
# then you can permute the order for vector of length N

# PARAMS: NUMERIC[] vec 
# OUTPUT: LIST of NUMERIC[]
# BRIEF: grow takes in a numeric vector of length N and returns a list of N+1 numeric vectors


grow <- function( vec ){
    Nplus1 <- length(vec)+1 # calculate N+1
    helperVec <- 1:Nplus1 # get our helper vector
    results <- vector("list", Nplus1 ) # allocate space for results
    for( i in 1:Nplus1 ){
        answerN <- numeric(Nplus1)
        answerN[ helperVec[i] ] <- Nplus1
        answerN[ helperVec[-i] ] <- vec
        results[[i]] <- answerN
        }
    return( results )
}

# PARAMS: CHARACTER[] letters 
# OUTPUT: CHARACTER[] 
# BRIEF: Function takes in a character vector of length N and returns a character vector of N! possible combination of the letters 

scrambleLetters <- function( letters ){
    letterLength <- length(letters)
    # declare the stack
    myStack <- Stack()
    myStack$push(1)
    # allocate space for answers
    answers <- character(factorial(letterLength))
    i <- 1
    while( !myStack$isEmpty()){
        top <- myStack$pop()
        topLen <- length(top)
        if( topLen == letterLength ){ # when lengths are ==
            string <- paste(letters[top], collapse = "")
            answers[i] <- string; i <- i+1  # it's time to write the string
        }
        else{
            grownList <- grow(top) # keep growing
            for( grown in grownList) myStack$push(grown)
        }
    }
    return(answers)
    
}


# grow(1) --> list( c(2,1), c(1,2) )
growTest_1 <- function(){
    expected <- list( 2:1, 1:2 )
    actual <- grow(1)
    pass <- if( is.logical(all.equal(actual, expected)) ) TRUE else FALSE
    stopifnot( pass )
    print( "grow(1) passed ") 
}

# grow( c(1,2) ) --> list( c(3,1,2), c(1,3,2), 3:1 )
growTest_2 <- function(){
    expected <- list( c(3,1,2), c(1,3,2), 1:3 )
    actual <- grow(1:2)
    pass <- if( is.logical(all.equal(actual, expected)) ) TRUE else FALSE
    stopifnot( pass )
    print( "grow(1:2) passed ") 
}


growTest_3 <- function(){
    expected <- list( c(4, 1, 2, 3), c(1, 4, 2, 3), 
                    c(1, 2, 4, 3), c(1, 2, 3, 4) )
    actual <- grow(1:3)
    pass <- if( is.logical(all.equal(actual, expected)) ) TRUE else FALSE
    stopifnot( pass )
    print( "grow(1:3) passed ") 
}

growTests <- function(){
    growTest_1()
    growTest_2()
    growTest_3()
}
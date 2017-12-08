source("envirohash.R")

Counter_ <- setRefClass("Counter_", 
                contains = "Set_",

                methods = list(

                # PARAMS: none 
                # RETURN: return the elements of the set
                # BRIEF: keys is a wrapper for enviroSet's elements() function 
                    keys = function() elements(),

                # PARAMS: CHARACTER key
                # RETURN: return the value assigned to the key in env
                # BRIEF: value is a wrapper for get. Returns error if none
                    value = function(key) get(key, env),

                # PARAMS: none 
                # RETURN: return the values assigned to the keys in env
                # BRIEF: analogous to key()
                    values = function() sapply( keys(), function(key) value(key) ),

                # PARAMS: CHARACTER[] keys  
                # RETURN: none
                # BRIEF: if key exists count += 1 otherwise count = 1
                    add = function(keys){
                        for( key in keys){
                            # some things already exist in our environment.. awkward
                            val <- if ( !has(key) ) 0 else value(key)
                            assign(key, val + 1, env)
                        }        
                    }

                )
)

# PARAMS: STR[] ... 
# RETURN: a Counter object
# BRIEF: create a counter object on a STR[] OR sequence of STR[] 
Counter <- function(...){
    myCounter <- Counter_$new( env = new.env() )
    if( nargs() > 0){ 
        arguments <- list(...) 
        for( item in arguments ) myCounter$add(item) 
    }
    return( myCounter )
}


# ======================= TODO ======================= #


# PARAMS: CHARACTER word  
# RETURN: Counter object 
# BRIEF: word is split into vector of letters and a 
# Counter is constructed on this character vector
LetterBag <- function(word) Counter(strsplit(word, "")[[1]])


# PARAMS: Counter A & Counter B  
# RETURN: BOOLEAN 
# BRIEF: returns TRUE if A is a subset of B, FALSE otherwise 
`%subset%` <- function(A, B){
    for (item in A$elements()){
        if( !B$has(item)) return ( FALSE)
        # if( !is.numeric(B$value(item) )) return(FALSE)
        if( B$value(item) < A$value(item)) return (FALSE)
    }
    return( TRUE ) 
    
} 

# Some test vecs and counters

testVec_1 <- c("a", "b", "c") # "c" is reserved for c() vector shit wtf 
testVec_2 <- c( testVec_1, "a", "a", "b" )
testVec_3 <- as.character( rep(1:4, 4:1) )
bigWord <- "abcdefghijklmnopqrztuvwxyz"
smallWord <- "sassafrass"

testCounter_1 <- Counter(testVec_1)
testCounter_2 <- Counter(testVec_2)
testCounter_3 <- Counter(testVec_3)

# ======================= TODO ======================= #


# LetterBag Tests Go Here
word1 <- c("crumbus")
lbTest_1 <- function(){
    lb <- LetterBag(word1)
    stopifnot( lb$value("c") == 1 )
    stopifnot( lb$value("r") == 1 )
    stopifnot( lb$value("u") == 2 )
    stopifnot( lb$value("m") == 1 )
    stopifnot( lb$value("b") == 1 )
    stopifnot( lb$value("s") == 1 )
    print("You Passed")
}
word2 <- c("stephope1a")
lbTest_2 <- function(){
    lb <- LetterBag(word2)
    stopifnot( lb$value("s") == 1 )
    stopifnot( lb$value("t") == 1 )
    stopifnot( lb$value("e") == 2 )
    stopifnot( lb$value("p") == 2 )
    stopifnot( lb$value("h") == 1 )
    stopifnot( lb$value("o") == 1 )
    stopifnot( lb$value("1") == 1 )
    stopifnot( lb$value("a") == 1 )
    print("You Passed")
}


# Subset Test

subsetTest <- function(){
    # these are subsets
    stopifnot( testCounter_1 %subset% testCounter_1 )
    stopifnot( testCounter_1 %subset% testCounter_2 )
    # these are not 
    stopifnot( !( testCounter_1 %subset% testCounter_3 ) )
    stopifnot( !( testCounter_2 %subset% testCounter_1 ) )
    print("Subset passes!")
}




# TESTS FOR COUNTER DATA STRUCTURE

    # Constructor
constructorTest <- function(){
    myCounter <- Counter(testVec_1, testVec_2, testVec_3)
    stopifnot( myCounter$has("a") )
    stopifnot( myCounter$value("a") == 4 )
    stopifnot( myCounter$has("b") )
    stopifnot( myCounter$value("b") == 3 )
    stopifnot( myCounter$has("c") )
    stopifnot( myCounter$value("c") == 2 )

    stopifnot( myCounter$has("1") )
    stopifnot( myCounter$value("1") == 4 )
    stopifnot( myCounter$has("2") )
    stopifnot( myCounter$value("2") == 3 )
    stopifnot( myCounter$has("3") )
    stopifnot( myCounter$value("3") == 2 )
    stopifnot( myCounter$has("4") )
    stopifnot( myCounter$value("4") == 1 )

    print( "constructor passed")

}


    # $keys()
keysTest <- function(){
    myCounter <- Counter()
    stopifnot( myCounter$keys() == character(0) ) # empty vector
    myCounter <- Counter(testVec_1)
    stopifnot( myCounter$keys() == myCounter$elements() )
    print( "keys passed" )
}

    # $add(keys)
# add is implicitly tested in other functions 

    # $value(key)
valueTest <- function(){
    myCounter <- Counter(testVec_2)
    stopifnot( myCounter$value("a") == 3 )
    stopifnot( myCounter$value("b") == 2 )
    stopifnot( myCounter$value("c") == 1 )
    print( "value passed" )
}

    # $values()
valuesTest <- function(){
    myCounter <- Counter(testVec_1)
    stopifnot( myCounter$values() == rep(1, 3) )
    print( "values passed" )
}

    # $wordToLetterBag()

CounterTests <- function(){
    constructorTest()
    keysTest()
    valueTest()
    valuesTest()
}



# NOW WRITE A SUBSET FUNCTION
    



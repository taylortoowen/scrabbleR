
# we're going to manipulate environments to create a hash set class(!!!)
# our hash set can only handle string which is limited but still super cool

# you will notice the bodies of the functions are quite short
# they are merely wrappers for the built-in environment functions
# they have existed this whole time right under our noses!


# ================================== SET CLASS ================================== #

Set_ <- setRefClass("Set_",
            fields = list(
                env = "ANY"
            ),
            
            methods = list(

                elements = function() ls(env),

                has = function(element){
                    if( exists(element, envir = env) ){
                        bound <- get(element, envir = env)
                        # letters c,q,t are bound to functions
                        return( !is.function( bound ) )
                    }
                    return( FALSE )
                },

                add = function(elements) for(item in elements) assign(item, NULL, env), # this can add one item or a vector

                delete = function(element) remove(list = element, envir = env)

            ) )

# =============================== SET CONSTRUCTOR =============================== #

# in this constructor we're going to use the dot-dot-dot (aka ellipses)
Set <- function(...){
    # the ellipses is used for a *variable* number of arguments
    mySet <- Set_$new( env = new.env() )
    # any function has nargs() which returns the number of formals (aka parameters)
    if( nargs() > 0){ # if we do Set() that's the empty set so you could skip this stuff
        arguments <- list(...) # turn the ... into a list
        for( item in arguments ) mySet$add(item) # loop through the list and any sub lists bc $add() is flexible ;)
    }
    return( mySet )
}


# ============================= SET USE EXAMPLES ============================= #

# here is a short example of possible uses of the set 

# # declare the empty set
# s <- Set()
# # add one element
# s$add("ok")
# stopifnot( s$has("ok") ) # ok is an existing element
# stopifnot( !s$has("what") ) # what does not exist
# # s$elements returns [1] "ok" because that's the only element
# # ***adding the same element multiple times doesn't do anything***
# s$add("ok")
# # you can only delete things that exist
# s$delete("ok") # lest you get an error
# # s$delete("ok") would return an error
# # ********** #
# # s$elements() to get all the items in the set
# stopifnot( identical( s$elements(), character(0) ) ) # the empty set
# # lets redeclare s and explore some of the constructor options
# s <- Set( as.character( 1:4 ) )
# chars <- c( '1', '2', '3' ,'4' )
# stopifnot( s$elements() == chars ) # constructor can take a vector
# s <- Set( '1', '4', '6' )
# chars <- c( '1', '4', '6' )
# stopifnot( s$elements() == chars ) # constructor can take individual strings
# s <- Set( as.character( 1:4 ), '1', '4', '6' )
# chars <- c( '1', '2', '3' ,'4', '6' )
# stopifnot( s$elements() == chars ) # constructor can take individual strings and vectors!!!



# ========================== SET OPERATIONS (TODO) ========================== #

# these will all be infix functions
# (see http://adv-r.had.co.nz/Functions.html @ Infix Functions if you need to freshen up)
# don't stress about it being an infix. it doesn't affect the body which is what you are defining

# HINTS: don't use built in set-operation functions like union or intersect
# try to stick to using Set $methods like add, has and delete
# don't overthink any of these functions.. they should only require a few lines of code (3-4, no more than 6)



# starting with an example of union
# A %U% B takes in two sets, A & B, and returns a third set C which is the union of the first two
`%U%` <- function(A, B){ 
    C <- Set() # create an empty set
    for( item in A$elements() ) C$add(item) # add from A
    for( item in B$elements() ) C$add(item) # add from B
    return( C ) # return set C, the union of set A and set B
}

# A %sub% B returns TRUE A is a subset of B (FALSE otherwise)
`%sub%` <- function(A, B){
    for (item in A$elements()){
        if( !B$has(item)){
        return ( FALSE)
       }   
    }
    return( TRUE ) 
    
} 

# super takes in A and B and says if A is a superset of B (how could you leverage sub??)
`%super%` <- function(A, B) B %sub% A 

# A += B will add all the elements in B into A but does not return anything
`%+=%` <- function(A, B) for( item in B$elements() ) A$add(item) 
    
 


# intersect operator (returns a set of shared elements)
`%I%` <- function(A, B){
    C <- Set()
    for (item in A$elements()) if( B$has(item)) C$add(item)
    return( C )
}

# exclude operator (returns a set of un-shared elemets )
# it's the opposite of intersect... A <- Set('a', 'b') & B <- Set('b', 'c').. A %E% B <- Set('a', 'c')
`%E%` <- function(A, B){
    C <- Set()
    for (item in A$elements()) if( !B$has(item)) C$add(item)
    for (item in B$elements()) if( !A$has(item)) C$add(item)
    return( C )
}

# TODO DEFINE ONE MORE OPERATOR / OPERATION 
# this operator must use delete somehow 
# perhaps a subtract function? keep it simple please
# also write appropriate tests for it

# ___ your function goes here ___
`%D%` <- function(A, B){
    for (item in A$elements()){
        if( B$has(item)){
            A$delete(item)
        }
    }
}

# # ========================== DEFINING TESTS ========================== #

#     # Union Test
# unionTest <- list(
#     message = "Union (aka %U%)",
#     funct = function(A,B) (A %U% B)$elements(),
#     As = list( Set(), Set('1'), Set('1','2'), Set('1', '2') ),
#     Bs = list( Set(), Set(), Set('1'), Set('1', '3') ),
#     expected = list( character(0), c('1'), c('1','2'), c('1', '2', '3') )
# )

#     # Sub Test
# subTest <- list(
#     message = "Subset (aka %sub%)",
#     funct = function(A,B) A %sub% B,
#     As = list( Set(), Set('1'), Set('1','2'), Set('1', '2') ),
#     Bs = list( Set(), Set(), Set('1'), Set('1', '3') ),
#     expected = c( T, F, F, F )
# )

#     # Super Test
# superTest <- list(
#     message = "Superset (aka %super%)",
#     funct = function(A,B) A %super% B,
#     As = list( Set(), Set('1'), Set('1','2'), Set('1', '2') ),
#     Bs = list( Set(), Set(), Set('1'), Set('1', '3') ),
#     expected = c( T, T, T, F )
# )
#     # Intersect Test
# intersectTest <- list(
#     message = "Intersect (aka %I%)",
#     funct = function(A,B) (A %I% B)$elements(),
#     As = list( Set(), Set('1'), Set('1','2'), Set('1', '2') ),
#     Bs = list( Set(), Set(), Set('1'), Set('1', '3') ),
#     expected = list( character(0), character(0), c('1'), c('1') )
# )

#     # Exclude Test
# excludeTest <- list(
#     message = "Exclude (aka %E%)",
#     funct = function(A,B) (A %E% B)$elements(),
#     As = list( Set(), Set('1'), Set('1','2'), Set('1', '2') ),
#     Bs = list( Set(), Set(), Set('1'), Set('1', '3') ),
#     expected = list( character(0), c('1'), c('2'), c('2', '3') )
# )

# # the state tests makes sure we're not changing A or B when we shouldn't
# # and we are changing it when we should be 

#     # union state test
# unionStateTest <- list(
#     message = "Union State",
#     funct = function(A,B){
#         C <- A %U% B
#         Alen <- length( A$elements() )
#         Blen <- length( B$elements() )
#         return( c( Alen, Blen) )
#     },
#     As = list( Set(), Set('1'), Set('1','2'), Set('1', '2') ),
#     Bs = list( Set(), Set(), Set('1'), Set('1', '3') ),
#     expected = list( c(0, 0), c(1, 0), c(2, 1), c(2, 2) )
# )

#     # intersect state test
# intersectStateTest <- list(
#     message = "Intersect State",
#     funct = function(A,B){
#         C <- A %I% B
#         Alen <- length( A$elements() )
#         Blen <- length( B$elements() )
#         return( c( Alen, Blen) )
#     },
#     As = list( Set(), Set('1'), Set('1','2'), Set('1', '2') ),
#     Bs = list( Set(), Set(), Set('1'), Set('1', '3') ),
#     expected = list( c(0, 0), c(1, 0), c(2, 1), c(2, 2) )
# )

#     # exclude state test
# excludeStateTest <- list(
#     message = "Exclude State",
#     funct = function(A,B){
#         C <- A %E% B
#         Alen <- length( A$elements() )
#         Blen <- length( B$elements() )
#         return( c( Alen, Blen) )
#     },
#     As = list( Set(), Set('1'), Set('1','2'), Set('1', '2') ),
#     Bs = list( Set(), Set(), Set('1'), Set('1', '3') ),
#     expected = list( c(0, 0), c(1, 0), c(2, 1), c(2, 2) )
# )

#     # += state test (this should change A's state!)
# plusEqualStateTest <- list(
#     message = "plusEqual State",
#     funct = function(A,B){
#         A %+=% B
#         Alen <- length( A$elements() )
#         Blen <- length( B$elements() )
#         return( c( Alen, Blen) )
#     },
#     As = list( Set(), Set('1'), Set('1','2'), Set('1', '2') ),
#     Bs = list( Set(), Set(), Set('1'), Set('1', '3') ),
#     expected = list( c(0, 0), c(1, 0), c(2, 1), c(3, 2) )
# )

# yourFunctionStateTest <- list(
#     message = "your function",
#     funct = function(A,B){
#         A %D% B
#         Alen <- length( A$elements() )
#         Blen <- length( B$elements() )
#         return( c( Alen, Blen) )
#     },
#     As = list( Set(), Set('1'), Set('1','2'), Set('1', '2') ),
#     Bs = list( Set(), Set(), Set('1'), Set('1', '3') ),
#     expected = list( c(0, 0), c(1, 0), c(1, 1), c(1, 2) )
# )


# outputTests <- list( unionTest, subTest, superTest, intersectTest, excludeTest)
# stateTests <- list( unionStateTest, intersectStateTest, excludeStateTest, plusEqualStateTest, yourFunctionStateTest)

# # ========================== TESTER FUNCTION ========================== #  

# # Test runner function
# runTests <- function(tests){
#     for ( test in tests ){

#         cat("\n")

#         isEqual <- function(A, B){
#             check <- all.equal(A, B)
#             if( is.logical(check) ) return(T) else return(F)
#         }

#         outputs <- Map(test$funct, test$As, test$Bs)
#         sames <- unlist( Map( isEqual , outputs, test$expected) )
#         fractionPassed <- paste0( as.character(sum(sames)), "/", as.character(length(test$As) ) )
        
#         cat(test$message, "Tests", fractionPassed, "\n") # print the message
        
#         for( i in seq_along(sames) ){
#             if ( !sames[i] ){
#                 # explain what happened
#                 cat(" Test", i, "failed", "\n")
#                 cat("  A:", test$As[[i]]$elements(), "\n")
#                 cat("  B:", test$Bs[[i]]$elements(), "\n")
#                 cat("  Expected:", test$expected[[i]], "\n")
#                 cat("  Recieved:", outputs[[i]], "\n" )
#             }
#         }
#     }
# }

# # ========================== EXECUTING TESTS ========================== # 

# # runTests( outputTests )
# # runTests( stateTests )


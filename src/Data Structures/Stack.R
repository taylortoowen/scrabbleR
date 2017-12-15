

stackNode <- setRefClass("MyNode",
fields = list(data = "ANY", nxt = "ANY"))

Stack_ <- setRefClass("Stack_",
        fields = list(len = "numeric", head = "ANY"),
        methods = list(
            
            
            push = function(anything){

                node <- stackNode$new(data = anything, nxt = NULL)
                len <<- len + 1 # incremenet 
                
                if ( is.null(head) ) {
                    head <<- node
                } else{
                    node$nxt <- head
                    head <<- node
                }   
            },

            pop = function(){
                if( isEmpty() ){
                    return(NULL)
                } else{
                    len <<- len - 1 # decrement 
                    node <- head
                    head <<- head$nxt
                    node$nxt <- NULL
                    return(node$data)
                }
            },

            isEmpty = function() return(len == 0)

        ))


# Stack constructor is a seperately defined function
# where the Stack_ ref class $new function is used
Stack <- function(){
    return(Stack_$new(len = 0, head = NULL))
}
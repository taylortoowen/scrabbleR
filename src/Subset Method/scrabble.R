#this is our environment
source("EnviroMap.R")
#this is our dictionary, the file names
smalldict.words <- "../../resources/smalldict.words"

bigdict.words <- "../../resources/ispell.words"
#these are the hands 
HandiBois <- c("a", "c", "t")

SpoonFingers <- c("h", "a", "n", "d")

Vorioblz <- c("d", "a", "t", "a")

loadDict <- function(filepath){
    # open the file for readign
    con <- file(filepath, "r")
    on.exit(close(con))
    #create new environment
#  scrabble <- new.env()
#     while ( TRUE ) {
#         line <- readLines(con, n = 1)
#         if ( length(line) == 0 ) {
#         break
#         }
#         lb <- LetterBag(line)
#         assign(line, lb, scrabble)
#     }
    #assigning word to count
    function(hand){
        newCount <- Counter(hand)
        #ls() lists all the items in the environment "scrabble"
        for( word in ls(scrabble)){
            #example of a trycatch to check errors
            tryCatch({
                #seeing if word is a subest of the counter
                if( get(word, scrabble) %subset% newCount) print(word)
            },
            #how to deal with errors
                error = function(cond){
                    cat("error on", word, "\n")
                }   
            )
            }
        }
}

#totally better
wayBetterFunction <- function(hand, filepath){
    newCount <- Counter(hand)
    con <- file(filepath, "r")
    newWord <- Counter(filepath)
    while ( TRUE ) {
        #n=1 so that it is one at a time
        line <- readLines(con, n=1)
        if (length(line) ==0 ){
            break
        }
        lb <- LetterBag(line)
        if( lb %subset% newCount) print(line)
    }
    close(con)
}

# badName <- loadDict(bigdict.words)
# badName(SpoonFingers)

# ld <- function(filepath){
#     # open the file for readign
#     con <- file(filepath, "r")
#     on.exit(close(con))
#     #create new environment
#     scrabble <- new.env()
#     while ( TRUE ) {
#         line <- readLines(con, n = 1)
#         if ( length(line) == 0 ) {
#         break
#         }
#         lb <- LetterBag(line)
#         assign(line, lb, scrabble)
#     }
#     return(scrabble)
# }
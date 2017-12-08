#this is our environment
source("EnviroMap.R")
#this is our dictionary, the file names
smalldict.words <- "smalldict.words"

bigdict.words <- "ispell.words"
#these are the hands 
HandiBois <- c("a", "c", "t")

SpoonFingers <- c("h", "a", "n", "d")

Vorioblz <- c("d", "a", "t", "a")

loadDict <- function(filepath){
    # open the file for readign
    con <- file(filepath, "r")
    on.exit(close(con))
    #create new environment
    scrabble <- new.env()
    while ( TRUE ) {
        line <- readLines(con, n = 1)
        if ( length(line) == 0 ) {
        break
        }
        lb <- LetterBag(line)
        assign(line, lb, scrabble)
    }
    #assigning word to count
    function(hand){
        newCount <- Counter(hand)
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

badName <- loadDict(bigdict.words)
badName(SpoonFingers)

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
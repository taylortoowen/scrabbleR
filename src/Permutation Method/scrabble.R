source("../Permutation Method/permuteSize.R")
source("../Permutation Method/permuteOrder.R")
source("../Data Structures/Envirohash.R")

smalldict.words <- "../../resources/smalldict.words"

bigdict.words <- "../../resources/ispell.words"

lettos <- c("r", "l", "p", "e", "o", "e")

mainFunction <- function(letters, dictionary){
    hashSet <- Set()
    possiblePermutations <- sapply( partitionSet(letters), function(w) scrambleLetters(w) )
    for( i in seq_along(possiblePermutations)){
        hashSet$add(possiblePermutations[[i]])
    }
    con <- file(dictionary, "r")
    while ( TRUE ) {
        #n=1 so that it is one at a time
        line <- readLines(con, n=1)
        if (length(line) ==0 ){
            break
        }
        if( hashSet$has(line)) print(line)
    }
    close(con)
}






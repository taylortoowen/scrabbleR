#this is our environment
source("../Data Structures/EnviroMap.R")
#this is our dictionary, the file names
smalldict.words <- "../../resources/smalldict.words"

bigdict.words <- "../../resources/ispell.words"
#these are the hands 
hand_1 <- c("a", "c", "t")

hand_2 <- c("h", "a", "n", "d")

hand_3 <- c("d", "a", "t", "a")

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
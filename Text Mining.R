library(readr)
library(ggplot2)
library(dplyr)
library(stringr)

invalid_characters <- c("--","\\?","\\!","\\.",",","\\.","'",":")
invalid_characters <- str_c(invalid_characters,  collapse = "|")

stopwords <- c("a", "about", "above", "above", "across", "after", "afterwards", "again", 
               "against", "all", "almost", "alone", "along", "already", "also","although",
               "always","am","among", "amongst", "amoungst", "amount",  "an", "and", "another", 
               "any","anyhow","anyone","anything","anyway", "anywhere", "are", "around", "as",  
               "at", "back","be","became", "because","become","becomes", "becoming", "been", 
               "before", "beforehand", "behind", "being", "below", "beside", "besides", 
               "between", "beyond", "bill", "both", "bottom","but", "by", "call", "can", 
               "cannot", "cant", "co", "con", "could", "couldnt", "cry", "de", "describe", 
               "detail", "do", "done", "down", "due", "during", "each", "eg", "eight", "either", 
               "eleven","else", "elsewhere", "empty", "enough", "etc", "even", "ever", "every", 
               "everyone", "everything", "everywhere", "except", "few", "fifteen", "fify", 
               "fill", "find", "fire", "first", "five", "for", "former", "formerly", "forty", 
               "found", "four", "from", "front", "full", "further", "get", "give", "go", "had", 
               "has", "hasnt", "have", "he", "hence", "her", "here", "hereafter", "hereby", 
               "herein", "hereupon", "hers", "herself", "him", "himself", "his", "how", "however", 
               "hundred", "ie", "if", "in", "inc", "indeed", "interest", "into", "is", "it", "its", 
               "itself", "keep", "last", "latter", "latterly", "least", "less", "ltd", "made", 
               "many", "may", "me", "meanwhile", "might", "mill", "mine", "more", "moreover", 
               "most", "mostly", "move", "much", "must", "my", "myself", "name", "namely", 
               "neither", "never", "nevertheless", "next", "nine", "no", "nobody", "none", "noone", 
               "nor", "not", "nothing", "now", "nowhere", "of", "off", "often", "on", "once", "one", 
               "only", "onto", "or", "other", "others", "otherwise", "our", "ours", "ourselves", 
               "out", "over", "own","part", "per", "perhaps", "please", "put", "rather", "re", 
               "same", "see", "seem", "seemed", "seeming", "seems", "serious", "several", "she", 
               "should", "show", "side", "since", "sincere", "six", "sixty", "so", "some", "somehow", 
               "someone", "something", "sometime", "sometimes", "somewhere", "still", "such", "system",
               "take", "ten", "than", "that", "the", "their", "them", "themselves", "then", 
               "thence", "there", "thereafter", "thereby", "therefore", "therein", "thereupon", 
               "these", "they", "thickv", "thin", "third", "this", "those", "though", "three", 
               "through", "throughout", "thru", "thus", "to", "together", "too", "top", "toward",
               "towards", "twelve", "twenty", "two", "un", "under", "until", "up", "upon", "us", 
               "very", "via", "was", "we", "well", "were", "what", "whatever", "when", "whence", 
               "whenever", "where", "whereafter", "whereas", "whereby", "wherein", "whereupon", 
               "wherever", "whether", "which", "while", "whither", "who", "whoever", "whole", 
               "whom", "whose", "why", "will", "with", "within", "without", "would", "yet", "you", 
               "your", "yours", "yourself", "yourselves", "the")

#Load File from the Internet :
f_pre <- readLines(file.choose())
str(f_pre)

f_pre[1:10]

#Question 1 : 	convert	 the	vector	of	lines	 to	a	vector	where	each	element	is	a	word

convert_to_words_vector <- function(w){
  y <- unlist(str_split(w, " "))
  y <- y[y != ""]
  return(y)
}

f_pre_vec <- convert_to_words_vector(f_pre)

#Question 2: 

#Function to do All the Operations :

word_converter <- function(f_pre_vec) {
  #Remove Invalid Characters
  f_post <- str_replace_all(f_pre_vec, invalid_characters, "")
  
  #Convert	each	word	to	lowercase
  f_post <- tolower(f_post) # or another Function is : f_post <- sapply(f_post, tolower)
  
  #Remove	all	stopwords	(contained	in	variable stopwords)
  f_post <- unlist(f_post)[!(unlist(f_post) %in% stopwords)]
  
  #Remove all Empty Strings
  f_post <- f_post[f_post != ""]
  
  return(f_post)
}

f_post <- word_converter(f_pre_vec)
str(f_post)

#3. point is a typo so attempting 4th Here
#4. Create	a	tibble	with	3	columns

ans <- tibble(word = unique(f_post))
ans <- ans %>% mutate(
  Pattern = str_c("^",word,"$"),
  WLength = str_length(word)
)
ans

#5. Create a Tibble that	 contains	the frequency	 of	word	length	occurrence for the text

freq <- ans[,3, drop = F]
freq <- freq %>% count(WLength)
colnames(freq)[1] <- "WLength"
colnames(freq)[2] <- "WFrequency"
freq

#6 Plot	the	results	for	the	chapter	text	analysis
ggplot(data = freq) + geom_line(mapping = aes(x = WLength, y = WFrequency)) + geom_point(mapping = aes(x = WLength, y = WFrequency),colour = "blue") + ylab("Word Frequency") + xlab("Word Length")
library(rvest)
library(tidyverse)
library(plotly)

library(knitr)
hook_output = knit_hooks$get('output')
knit_hooks$set(output = function(x, options) {
  # this hook is used only when the linewidth option is not NULL
  if (!is.null(n <- options$linewidth)) {
    x = knitr:::split_lines(x)
    # any lines wider than n should be wrapped
    if (any(nchar(x) > n)) x = strwrap(x, width = n)
    x = paste(x, collapse = '\n')
  }
  hook_output(x, options)
})




# Read a webpage --------------------------------------------------------

library(rvest)
author_url <- "https://en.wikipedia.org/wiki/Tim_Winton"
wiki_data <- read_html(author_url) # Scrape the data from the webpage
wiki_data


# How to scrape a table - html_table() ------------------------------------

table_data <- wiki_data %>% 
  rvest::html_table() #Get all tables on the webpage

length(table_data)
str(table_data[[1]])


# Other approaches - html_nodes() -----------------------------------------

table_data_eg1 <- wiki_data %>%
  rvest::html_nodes("table") %>% # get all the nodes of type table
  purrr::pluck(1) %>% #pull out the first one
  rvest::html_table(header = FALSE) #convert it to table type
str(table_data_eg1)


# Other approaches - html_node() ------------------------------------------

table_data_eg2 <- wiki_data %>%
  rvest::html_node("table") %>% # just get the first table match
  rvest::html_table(header = FALSE) #convert it to table type
str(table_data_eg2)


# Get the nationality -----------------------------------------------------

author_nationality = table_data_eg2 %>%
  dplyr::rename(Category = X1, Response = X2) %>%
  dplyr::filter(Category == "Nationality") %>%
  dplyr::select(Response) %>%
  as.character()
author_nationality

# Same example -  Different author ----------------------------------------

"https://en.wikipedia.org/wiki/Jane_Austen"

author_first_name = "Jane"
author_last_name = "Austen"
author_url <- paste("https://en.wikipedia.org/wiki/", 
                    author_first_name, "_", author_last_name, sep = "")
wiki_data <- read_html(author_url)

## Let's get that table

table_again <- wiki_data %>%
  rvest::html_nodes(".infobox.vcard") %>% #search for a class
  rvest::html_table(header = FALSE) %>% 
  purrr::pluck(1) 
head(table_again)

## Web scraping is tricky

table_again %>% dplyr::select(X1) %>% unlist() %>% as.vector()


## Differnt way - Matching paragraphs

para_data <- wiki_data %>%
  rvest::html_nodes("p") # get all the paragraphs
head(para_data)

## Get the text - html_text()

text_data <- para_data %>%
  purrr::pluck(2) %>% # get the second paragraph
  rvest::html_text() # convert the paragraph to text
head(text_data)


# Xpath example -----------------------------------------------------------

## Using an Xpath

para_xpath = '//*[@id="mw-content-text"]/div/p[2]'
text_data <- wiki_data %>%
  rvest::html_nodes(xpath = para_xpath) %>%
  rvest::html_text()
text_data

## JSpath Example

## Using CSS ID

para_css = "#mw-content-text > div > p:nth-child(5)"
text_data <- wiki_data %>%
  rvest::html_nodes(css = para_css) %>%
  rvest::html_text()
text_data


# Text analysis -----------------------------------------------------------

possible_nationalities <- c("Australian", "Chinese", "Mexican", "English", "Ethiopian")

# Do any of these nationalities appear in the text?
count_values = str_count(text_data, possible_nationalities)
possible_nationalities[count_values == TRUE]


# Our question ------------------------------------------------------------

# Get our list

## 1001 books to read

## Read the book list from a website
  
book_list_url <- "https://mizparker.wordpress.com/the-lists/1001-books-to-read-before-you-die/"
paragraph_data <- read_html(book_list_url) %>% # read the web page
  rvest::html_nodes("p") # get the paragraphs
head(paragraph_data)


## Get the book list from the paragraphs 

book_string <- paragraph_data %>%  #the list is in pieces
  purrr::pluck(4) %>% # get the first part of the list
  html_text(trim = TRUE) %>% # convert it to text, remove white space
  gsub("\n", "", .) #remove the newline character
head(book_string)

## Let's put our list together

strsplit("a123b", split = "\\d") 
#Split by digits \\d
strsplit("a123b", split = "\\d+") 
#Split by one or more digits \\d+
strsplit("a.b", split = "\\.") 
#Split by fullstop \\.
strsplit("a1.b", split = "\\d+\\.") 
#Split by digits and fullstop \\d+\\.
strsplit("a1.b", split = "\\d+?\\.") 
#Matches as few digits as possible \\d+? and fullstop \\.

## Split up our list
  
split_book_string <- book_string %>% 
  strsplit(split = "\\d+?\\.") %>% 
  # split the string by any numbers followed by a full stop
  as.data.frame(stringsAsFactors = FALSE) %>% 
  # make this a data frame
  dplyr::filter(. != "")  

# remove any empty rows

head(split_book_string)

## Split up our columns 

names(split_book_string) <- "book_string"
book_df <-split_book_string %>%
  tidyr::separate(book_string, sep = "\\–", into = c("book", "author"))
# split our author and book into columns
# very lucky that whoever coded this webpage used a long hash!
head(book_df)

## Wrap the code chunks

Get_book_data <- function(para_ind){
    book_str <- paragraph_data %>%
    purrr::pluck(para_ind) %>%
    html_text(trim = TRUE) %>% 
    gsub("\n", "", .)  #remove newline character
  
  book_df <- book_str %>% 
    strsplit(split = "\\d+?\\.") %>% #match the number index
    as.data.frame(stringsAsFactors = FALSE) %>% 
    dplyr::filter(. != "")  # remove empty first row
  
  names(book_df) <- "book_string"
  book_df <- book_df %>%
    tidyr::separate(book_string, sep = "\\–", into = c("book", "author"))
  
  return(book_df)
  
}
```

## Put it together

book_data <- lapply(seq(4,12,2) %>% as.list(), Get_book_data) %>% 
  do.call(rbind, .) %>% 
  dplyr::mutate(author = str_trim(author))

nrow(book_data) # Has 1001 rows so let's assume we are all good!

head(book_data) # Looks pretty good at first glance

# Get nationalities

## More wrapping

Read_wiki_page <- function(search_string){
  search_string = gsub("\\s+", "_", search_string)
  wiki_url <- paste("https://en.wikipedia.org/wiki/", 
    search_string, sep = "")
  wiki_data <- read_html(wiki_url)
  return(wiki_data)
}

Get_wiki_infocard <- function(wiki_data){
  infocard <- wiki_data %>%
    rvest::html_nodes(".infobox.vcard") %>%
    rvest::html_table(header = FALSE, fill = TRUE) %>% 
    purrr::pluck(1)
  return(infocard)
}

Get_nationality_from_infocard <- function(infocard){
  nationality <- infocard %>%
    dplyr::rename(Category = X1, Response = X2) %>%
    dplyr::filter(Category == "Nationality") %>%
    dplyr::select(Response) %>%
    as.character()
  return(nationality)
}

Get_first_text <- function(wiki_data){
  paragraph_data <- wiki_data %>%
    rvest::html_nodes("p")
  i = 1
  no_text = TRUE
  while(no_text){
    text_data <- paragraph_data %>%
      purrr::pluck(i) %>% 
      rvest::html_text() 
    check_text = gsub("\\s+", "", text_data)
    if(check_text == ""){
      i = i + 1
    }else{
      no_text = FALSE
    }
  }
  return(text_data)
}

Guess_nationality_from_text <- function(text_data, possible_nationalities){
  num_matches <- str_count(text_data, possible_nationalities)
  prob_matches <- num_matches/sum(num_matches)
  i = which(prob_matches > 0)
  if(length(i) == 1){
    prob_nationality = possible_nationalities[i] 
  }else if(length(i) > 0){
    warning(paste(c("More than one match for the nationality:", 
                  possible_nationalities[i], "\n"), collapse = " "))
    match_locations = str_locate(text_data, possible_nationalities[i])
    j = i[which.min(match_locations[,1])]
    prob_nationality = possible_nationalities[j] 
  }else{
    return(NA)
  }
  return(prob_nationality)
}

Query_nationality_from_wiki <- function(search_string, possible_nationalities = NULL){
  wiki_data <- Read_wiki_page(search_string)
  infocard <- Get_wiki_infocard(wiki_data)
 if(is.null(infocard)){
    nationality <- "Missing infocard"
  }else if(any(infocard[,1] == "Nationality")){
    nationality <- Get_nationality_from_infocard(infocard)
  }else{
    first_paragraph <- Get_first_text(wiki_data)
    nationality <- Guess_nationality_from_text(first_paragraph,
      possible_nationalities)
  }
  return(nationality)
}

search_string = "Tim Winton"
wiki_data <- Read_wiki_page(search_string)
infocard <- Get_wiki_infocard(wiki_data)
if(is.null(infocard)){
  nationality <- "Missing infocard"
}else if(any(infocard[,1] == "Nationality")){
  nationality <- Get_nationality_from_infocard(infocard)
}else{
  first_paragraph <- Get_first_text(wiki_data)
  nationality <- Guess_nationality_from_text(first_paragraph,
    possible_nationalities)
}
nationality

## What nationalities to search for?

# Get table of nationalities
url <- "http://www.vocabulary.cl/Basic/Nationalities.htm"
xpath <- "/html/body/div[1]/article/table[2]"
nationalities_df <- url %>%
  read_html() %>%
  html_nodes(xpath = xpath) %>%
  html_table() %>% 
  as.data.frame()

possible_nationalities = nationalities_df[,2]
head(possible_nationalities)

## Manual fixing

fix_entry = "ArgentineArgentinian"
i0 = which(nationalities_df == fix_entry, arr.ind = TRUE)
new_row = nationalities_df[i0[1], ]
nationalities_df[i0] = "Argentine"
new_row[,2] = "Argentinian"
nationalities_df = rbind(nationalities_df, new_row)

fix_footnote1 = "Colombia *"
i1 = which(nationalities_df == fix_footnote1, arr.ind = TRUE)
nationalities_df[i1] = strsplit(fix_footnote1, split = ' ')[[1]][1]

fix_footnote2 = "American **"
i2 = which(nationalities_df == fix_footnote2, arr.ind = TRUE)
nationalities_df[i2] = strsplit(fix_footnote2, split = ' ')[[1]][1]

possible_nationalities = nationalities_df[,2]

## Get Nationalities

nationality_from_author_search = sapply(book_data$author[1:20],  
                                        function(search_string){
                                          nataionality = tryCatch( # Just in case!
                                            Query_nationality_from_wiki(search_string, 
                                                                        possible_nationalities),
                                            error = function(e) NA)
                                        }) %>% unlist()
nationality_from_author_search

# How diversely do we read

## Run it!

nationality_from_author_search = sapply(book_data$author %>% unique(),  
                                        function(search_string){
                                          print(search_string)
                                          nataionality = tryCatch( # Just in case!
                                            Query_nationality_from_wiki(search_string, 
                                                                        possible_nationalities),
                                            error = function(e) NA)
                                        }) 

author_nationality_df <- as.data.frame(nationality_from_author_search) %>%
  dplyr::mutate(author = rownames(.))

names(author_nationality_df) <- c("nationality", "author")

book_data <- book_data %>%
  dplyr::left_join(author_nationality_df)

head(book_data)

save(book_data, file = "book_data.RData")

## Result

load("book_data.RData")

table_nationalities <- book_data %>% 
  dplyr::select(author, nationality) %>%
  dplyr::distinct() %>%
  dplyr::select(nationality) %>%
  unlist() %>% 
  table() %>% 
  as.data.frame(stringsAsFactors = FALSE)

names(table_nationalities ) = c("Nationality", "Frequency")

table_nationalities %>% 
  arrange(desc(Frequency))

head(table_nationalities)

## Let's take a look

pie_plot <- table_nationalities %>%
  plot_ly(labels = ~Nationality, values = ~Frequency) %>%
  add_pie(hole = 0.6) %>%
  layout(title = "Nationalities",  showlegend = F,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

## Plotting result

pie_plot


##### Dutch eBay Car Comparer #####
# Compares cars on Marktplaats.nl (the Dutch eBay) by personalized queries.
# The final dataframe carries the column 'Bargain' that indicates whether your car has good relative mileage, age and price within the search query.

#### Search Parameters ####
#edit these fields to specify the query
dist = '100000' #in meters, quotations are required
max_year = 2016 
min_year = 2009
min_mileage = 20000 #in km
max_mileage = 150000 #in km
min_price = 2000 #in euro
max_price = 5000 #in euro
postcode = '1064EW'
bedrijf = F #include cars from a bedrijf (company) seller

#### Functions ####
simpleCap <- function(x) {
  #makes the first letter of a string to capital
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
} #caps first letter of words in vector or/and string
trim <- function(x) {gsub("^\\s+|\\s+$", "", x)}
get_n_results <- function(bedrijf) {
  #return the number of total results of query
  query = make_query(1,bedrijf,dist,min_year,max_year,min_price,max_price,min_mileage,max_mileage,postcode)
  results = read_html(query)
  results_tag = 'div.mp-Card-body'
  html_fields <- results %>% html_nodes(results_tag)
  results = xml_text(html_fields)[1]
  results_index = unlist(gregexpr('[0-9]',results))
  n = as.numeric(substr(results,results_index[1],results_index[length(results_index)]))
  n = as.numeric(gsub('\\.','',n)) #needed as MP returns values over 1k with '.'
  return(n)
}
get_page_df <- function(query){
  ## page parser ##
  page <- read_html(query)
  
  #parse titles
  title_tag <- 'span.mp-listing-title'
  html_fields <- page %>% html_nodes(title_tag)
  title <- xml_text(html_fields)
  
  #parse price
  price_tag <- 'div.price-new.ellipsis'
  html_fields <- page %>% html_nodes(price_tag) 
  price <- xml_text(html_fields)
  price <- trim(gsub('\u20AC','',gsub(',','.',gsub('\n','',price))))
  price <- gsub('\342\202\254\302\240','',price) #required for daemon robustness, the euro sign sucks
  price[which(price=='Bieden')] <- 0
  price[which(price=='Gereserveerd')] <- NA
  price <- unlist(sapply(price,function(x){x=substr(x,1,nchar(x)-3)},USE.NAMES = F)) #remove '.' and cents
  price <- gsub('\\.','',price)
  
  #parse location --> maybe this is a tell for the right sellers, as bezorgt in are companies
  location_tag <- 'div.location-name'
  html_fields <- page %>% html_nodes(location_tag) 
  location <- xml_text(html_fields)
  
  #parse distance (in km)
  distance_tag <- 'div.distance'
  html_fields <- page %>% html_nodes(distance_tag)
  distance <- as.numeric(gsub(' km','',xml_text(html_fields)))
  
  #parse bouwjaar & mileage
  info_tag <- 'span.mp-listing-attributes'
  html_fields <- page %>%  html_nodes(info_tag)
  age <- as.numeric(format(Sys.Date(),'%Y')) -  as.numeric(xml_text(html_fields)[seq(1,length(distance)*2,2)])
  mileage <- xml_text(html_fields)[seq(2,length(distance)*2,2)]
  mileage <- sapply(mileage,function(x){x = substr(x,1,nchar(x)-3)}) #remove km
  mileage <- gsub('\\.','',mileage)
  
  #count topadverties (to be removed later)
  top_tag <- 'span.mp-listing-priority-product'
  html_fields <- page %>%  html_nodes(top_tag)
  top <- xml_text(html_fields)
  top_count <- length(which(top == 'Topadvertentie'))
  
  #id
  id_tag <- 'article'
  html_fields <- page %>%  html_nodes(id_tag)
  id <- html_attr(html_fields,name='data-url')
  
  attributes = list('title'= title,'price' = price,'location' = location,'distance' = distance,'age' = age,'mileage' = mileage,'top' = top,'id'= id)
  names(attributes) <- sapply(names(attributes),simpleCap)
  
  #remove the dagtoppers only when they are present
  if (length(which(attributes$Top=='Dagtopper')) > 0){
    attributes = lapply(attributes,function(x){x=x[-which(attributes$Top=='Dagtopper')]})}
  
  #create df
  data <- data.frame(matrix(unlist(attributes),nrow=length(attributes$Title), byrow=F))
  colnames(data) <- names(attributes)
  data <- data[ , -which(names(data) %in% c("Top"))]
  data$Mileage <- as.numeric(as.character(data$Mileage))
  data$Price <- as.numeric(as.character(data$Price))
  data$Age <- as.numeric(as.character(data$Age))
  data$Bargain <- as.integer(1 / (data$Age/max(data$Age) *(data$Price/max(data$Price))^2 * data$Mileage/max(data$Mileage)))
  data <- data[,c(1:6,8,7)]
  row.names(data) <- NULL
  return(data)
}
make_query <- function(i,bedrijf,dist,min_year,max_year,min_price,max_price,min_mileage,max_mileage,postcode) {
  #change query for page number 'i'
  
  if (bedrijf == T){
    query = paste('http://www.marktplaats.nl/z.html?attributes=S%2C10898&attributes=S%2C10899&attributes=S%2C10882&attributes=N%2C189&attributes=N%2C190&attributes=N%2C191&attributes=S%2C481&priceFrom=',min_price,'%2C00&priceTo=',max_price,'%2C00&yearFrom=',min_year,'&yearTo=',max_year,'&mileageFrom=',min_mileage,'&mileageTo=',max_mileage,'&categoryId=91&postcode=',postcode,'&distance=',dist,'&currentPage=',i,'&numberOfResultsPerPage=100',sep='')
  }else {query = paste('http://www.marktplaats.nl/z.html?attributes=S%2C10898&attributes=S%2C10882&attributes=N%2C189&attributes=N%2C190&attributes=N%2C191&attributes=S%2C481&priceFrom=',min_price,'%2C00&priceTo=',max_price,'%2C00&yearFrom=',min_year,'&yearTo=',max_year,'&mileageFrom=',min_mileage,'&mileageTo=',max_mileage,'&categoryId=91&postcode=',postcode,'&distance=',dist,'&currentPage=',i,'&numberOfResultsPerPage=100',sep='')}
  return(query)
} #pas hier de zoekterm aan
make_df <- function(n,j,bedrijf) {
  for (i in 1:j){
    #print(i)
    query = make_query(i,bedrijf,dist,min_year,max_year,min_price,max_price,min_mileage,max_mileage,postcode)
    new_data = get_page_df(query)
    if (i == 1){data = new_data}
    else {data = rbind(data,new_data)}
  }
  row.names(data) <- NULL
  return(data)
}

#### Setup ####
library(rvest)
library(XML)
n = get_n_results(bedrijf) #number of results
j = ceiling(n/100) #number of pages contained by results

#### MAIN ####
data <- make_df(n,j,bedrijf)
data <- data[with(data, order(-data$Bargain)), ]; row.names(data) <- NULL
rm(list=setdiff(ls(), "data"))

#### Filter ####
filter_data <- data[grepl('Fiat|Panda|fiat|panda',data$Title),]

#### Plot ####
plot(data$Age,data$Mileage)
plot(data$Age,data$Price)
plot(data$Mileage,data$Price)

#'http://www.marktplaats.nl/z.html?attributes=S%2C10898&attributes=S%2C10882&attributes=N%2C189&attributes=N%2C190&attributes=N%2C191&attributes=S%2C481&priceFrom=1.000%2C00&priceTo=5.000%2C00&yearFrom=2006&yearTo=2016&mileageFrom=10.000&mileageTo=150.000&categoryId=91&postcode=1064EW&distance=50000&currentPage=',i,'&numberOfResultsPerPage=100',sep=''
#'http://www.marktplaats.nl/z.html?attributes=S%2C10898&attributes=S%2C10899&attributes=S%2C10882&attributes=N%2C189&attributes=N%2C190&attributes=N%2C191&attributes=S%2C481&priceFrom=1.000%2C00&priceTo=5.000%2C00&yearFrom=2006&yearTo=2016&mileageFrom=10.000&mileageTo=150.000&categoryId=91&postcode=1064EW&distance=50000&currentPage=',i,'&numberOfResultsPerPage=100',sep=''

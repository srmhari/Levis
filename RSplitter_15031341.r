#call from VBA, path parameter will be passed
args = commandArgs(trailingOnly=TRUE)

workingDir = args[1]
print (workingDir)
configFile <- paste(workingDir, "config.R", sep="/")
source(configFile)

#input_filename <- paste(workingDir, fileName, sep="/")
input_filename <-args[2]
con <- file(description=input_filename,open="r")


sales_data <- tryCatch({
  read.table(con,sep=delim,nrows=cutoff,quote="\"", fill=TRUE, header=TRUE, na.strings = "#EMPTY", comment.char = "")
}, error=function(err) {
  ## matching condition message only works when message is not translated
  if (identical(conditionMessage(err), "no lines available in input"))
    data.frame()
  else {
    writeLines(c(err), fileConn)
    print(err)
    Sys.sleep(2)
    close(fileConn)
  }
})
sales_data_ncols = ncol(sales_data)

header_ncols <- length(header)

if (sales_data_ncols == header_ncols) {
  colnames(sales_data) = header 
}else if (sales_data_ncols - header_ncols == 2) {
  colnames(sales_data) = c(header,place_holders)
}else if (sales_data_ncols - header_ncols == 1) {  
  colnames(sales_data) = c(header,"Place.Holder.1")
}
#print colnames(sales_data)
#sys.sleep(2)
index = 1
write.csv(sales_data, paste(workingDir,paste("input",index,".csv",sep=""),sep="/"),  row.names = FALSE)
#sales_data_ncols = ncol(sales_data)
print(sales_data_ncols)
#Sys.sleep(2)
#header_ncols <- length(header)
sales_data_cols = colnames(sales_data)

error_code = 0
fileConn<-file(paste(workingDir, "RSplitter_status.txt", sep="/"),open="w")
if (sales_data_ncols != header_ncols){
  if((sales_data_ncols-header_ncols <= 2) & (all(sort(head(sales_data_cols,header_ncols)) == sort(header)))){
    if(all(head(sales_data_cols,header_ncols) == header)){
      error_code = sales_data_ncols-header_ncols
    }else{
      error_code = -2
      writeLines(c("Columns are arranged in unsuitable way, please give them in correct order"), fileConn)
      close(fileConn)
      stop("Columns are arranged in unsuitable way, please give them in correct order")
    }
  }else {
    error_code = -1
    writeLines(c("Invalid number of columns, please check your input file"), fileConn)
    close(fileConn)
    stop("Invalid number of columns, please check your input file")
  }
} else if(!(all(sales_data_cols == header))){
  if(all(sort(sales_data_cols) == sort(header))){
    error_code = -2
    writeLines(c("Columns are arranged in unsuitable way, please give them in correct order"), fileConn)
    close(fileConn)
    stop("Columns are arranged in unsuitable way, please give them in correct order")
  }
  else {
    error_code = 0
  }
}

perm_header = header
if (sales_data_ncols == header_ncols){
  colnames(sales_data) <- header
}else if(sales_data_ncols - header_ncols == 2){
  colnames(sales_data) <- c(header,place_holders)
  #temp_header <- c(temp_header,geo_place_holders)
  perm_header <- c(header,place_holders)
}else if(sales_data_ncols - header_ncols == 1){
  colnames(sales_data) <- c(header,head(place_holders,1))
  #temp_header <- c(temp_header,head(geo_place_holders,1))
  perm_header <- c(header,head(place_holders,1))
}

x = list(c(),
         c(),
         c(),
         c(),
         c(),
         c(),
         c(),
         c())
if(error_code == 1){
  #x9 <- c()
  x = append(x,list(c()))
}else if(error_code == 2){
  #x9 <- c()
  #x10 <- c()
  x = append(x,list(c(),c()))
  
}

distinct_stores <- unique(sales_data[determine_unique_stores])
door_attributes <- read.csv(door_attr_filename)
#print nrow(door_attributes)
repeat{
  
  d1 <- tryCatch({
    merge(sales_data, door_attributes, by.x = matching_column_sales, by.y = matching_column_door, suffixes = c("","2"), all.x = FALSE, all.y = TRUE)
  }, error=function(err) {
    print("Error:")
    print(err)
    Sys.sleep(5)
    writeLines(c(err), fileConn)
    close(fileConn)
  })
  print(nrow(d1))
  d2 <- d1[,c(temp_header)]
  df3 <- d2[!duplicated(d2),]
  
  
  if (index == 1){
    d3 <- df3 
    
  }
  else{
    d3=rbind(d3,df3)
    y = unique(sales_data[determine_unique_stores])
    distinct_stores = rbind(distinct_stores,y)
  }
  x[[1]] <- c(x[[1]],levels(factor(sales_data$Reporting.Period.Fiscal.Year.Week)))
  x[[2]] <- c(x[[2]],levels(factor(sales_data$Reporting.Period.Fiscal.Year.Week)))
  x[[3]] <- c(x[[3]],levels(factor(sales_data$Brand...SAP.Detail)))
  x[[4]] <- c(x[[4]],levels(factor(sales_data$Product.Category...Detail)))
  x[[5]] <- c(x[[5]],levels(factor(sales_data$Product.Subcategory...Detail)))
  x[[6]] <- c(x[[6]],levels(factor(sales_data$Consumer.Segment...Detail)))
  x[[7]] <- c(x[[7]],levels(factor(sales_data$Consumer.Group.Extension2...Detail)))
  x[[8]] <- c(x[[8]],levels(factor(sales_data$Country)))
  
  if(error_code == 1){
    x[[9]] <- c(x[[9]],levels(factor(sales_data$Place.Holder.1)))
  }else if(error_code == 2){
    x[[9]] <- c(x[[9]],levels(factor(sales_data$Place.Holder.1)))
    x[[10]] <- c(x[[10]],levels(factor(sales_data$Place.Holder.2)))
  }
  
  
  if(nrow(sales_data) == 0){
    break
  }
  if (nrow(sales_data) != cutoff){
    break
  }
  data <- tryCatch({
    sales_data <- read.table(con, nrows=cutoff, header=F, fill=TRUE, sep=delim,skip=0,na.strings = "#EMPTY", comment.char = "",quote="\"")
    index = index + 1
    colnames(sales_data) <- perm_header
    write.csv(sales_data, paste(workingDir,paste("input",index,".csv",sep=""),sep="/"),  row.names = FALSE)
  }, error=function(err) {
    ## matching condition message only works when message is not translated
    if (identical(conditionMessage(err), "no lines available in input"))
      data.frame()
    else {
      writeLines(c(err), fileConn)
      close(fileConn)
    }
  })
  
}

for(i in 1:10){
x[[i]] <- unique(x[[i]])
}
len <-list(length(x[[1]]))
for(i in 2:10){
len = append(len,list(length(x[[i]])))
}

max_rows = max(unlist(len))

start.date <- c(1:max_rows)
end.date <- c(1:max_rows)
brand <- c(1:max_rows)
product.category <- c(1:max_rows)
product.subcategory <- c(1:max_rows)
consumer.segment <- c(1:max_rows)
consumer.group <- c(1:max_rows)
country <- c(1:max_rows)
place.holder.1 <- c(1:max_rows)
place.holder.2 <- c(1:max_rows)
stores.count <- c(1:max_rows)
#add country at the last of the data frame
user_option <-data.frame(start.date,end.date,brand,product.category,product.subcategory,consumer.segment,consumer.group,country,place.holder.1, place.holder.2,stores.count)
user_option[,c(1:ncol(user_option))] <- NA

distinct_stores_cnt <- nrow(unique(distinct_stores))
#brand
for (i in 1:10){
  for (j in 1: max(len[[i]])){
    user_option[,i][j] = x[[i]][j]
  }
}

user_option$stores.count[1] <- distinct_stores_cnt
#file written in the directory in CSV format
indicate_completion <- paste(workingDir,"success.csv",sep = "/")
write.csv(user_option,indicate_completion,row.names = FALSE,na="")
temp_output_name_path <- paste(workingDir,"temp.csv",sep = "/")
write.csv(d3,temp_output_name_path,row.names = FALSE)
writeLines(c("File Splitter ran successfully"), fileConn)
close(con)
close(fileConn)
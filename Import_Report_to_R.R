
##########################################################
##
## Import Clifton Strengths - first Page#
## using R Language into SQL Server table or R Data.frame
##
##########################################################


library(pdftools)
library(stringr)
library(RODBC)
library(tidyverse)


###########################
### Helper function
###########################

# SQL Table Schema: CREATE TABLE StrengthReport (Name VARCHAR(100), StrengthName VARCHAR(50), StrengthEnum INT)
insert_to_SQL <- function(EmpName, StrName, StrEnum) {
  con <- odbcDriverConnect('driver={SQL Server};server=localhost;database=Clifton_db;trusted_connection=true')
  QueryText2 <- paste("INSERT INTO StrengthReport (Name, StrengthName, StrengthEnum) SELECT '",EmpName,"', '",StrName,"',",StrEnum,"")
  sqlQuery(con,QueryText2)
  close(con)
}


insert_to_dataframe <- function(fnName, fnStrN, fnStrE) {

  df <<- df %>% add_row(Name = fnName, StrengthName = fnStrN, StrengthEnumerator = fnStrE)
  #df <<- rbind(df, setNames(data.frame(Name = fnName, StrengthName = fnStrN,StrengthEnumerator = fnStrE),colnames(df)))
}


### Dataframe for import
df <- data.frame(Name=character(), 
                  StrengthName=character(), 
                  StrengthEnumerator=integer()
)



###########################
###########################


insert_PDF_first_page <- function(username, filename) {
  path <- as.character('')
  
  # List of values
  val34 <- c("Achiever","Learner","Individualization","Significance","Relator"
             ,"Positivity","Ideation","Futuristic","Maximizer","Arranger","Self-Assurance"
             ,"Intellection","Strategic","Input","Command","Focus","Activator"
             ,"Responsibility","Adaptability","Competition","Communication","Connectedness","Woo"
             ,"Analytical","Belief","Empathy","Developer","Discipline","Includer","Context","Restorative"
             ,"Deliberative","Harmony","Consistency")
  
  
  filenamepath <- paste0(as.character(path),as.character(filename), collapse=NULL)
  filenamepath <- normalizePath(filenamepath)
  list_output <- pdftools::pdf_text(filenamepath) 
  raw_text <- list_output[[1]]
  
  
  #txt 1.page
  table <- stringr::str_split(raw_text[[1]], "\n", simplify = TRUE) ###  win "\r"
  table <- data.frame(table)
  
  #Name and date
  table$X1 <- str_squish(table$X1)
  loc_sep <- which(strsplit(table$X1, "")[[1]]=="|")
  namesurname <- substr(table$X1,1,loc_sep-1)
  dateTest <- substr(table$X1,loc_sep+1, nchar(table$X1))
  
 
  # find starting position
  start_pos_double <- as.integer(which(sapply(table, function(x) {str_detect(x, "1. ")})==TRUE))[1]
  start_pos_single <- as.integer(which(sapply(table, function(x) {str_detect(x, "21. ")})==TRUE))[1]
  
  double_st <- function(){
    x <- 1
    y <- 11
    df_str <- data.frame(ord = integer(), stre = character(), stringsAsFactors = FALSE)
    end_st <- start_pos_single-1
    
    for(i in start_pos_double:end_st){
      names <- colnames(table)[i]
      st1 <- table[names]
      str1 <- sapply(strsplit(str_squish(st1[[1]]), "\\s+"), "[", 2)
      df_str <- rbind(df_str, data.frame(ord=x, stre=str1))
      
      str11 <- sapply(strsplit(str_squish(st1[[1]]), "\\s+"), "[", 4)
      df_str <- rbind(df_str, data.frame(ord=y, stre=str11))
      x <- x + 1
      y <- y + 1
    }
    df_str_final <<- df_str
  }
  
  single_st <- function(){
    z <- 21
    df_str <- data.frame(ord = integer(), stre = character(), stringsAsFactors = FALSE)
    tab_len <- length(table)-2
    for(i in start_pos_single:tab_len){
      names <- colnames(table)[i]
      st2 <- table[names]
      str2 <- sapply(strsplit(str_squish(st2[[1]]), "\\s+"), "[", 2)
      df_str <- rbind(df_str, data.frame(ord=z, stre=str2))
      z <- z + 1
    }
    df_str_final <<- rbind(df_str_final, df_str)
  }
  double_st()
  single_st()
  df_str_final
  df_str_final$ord <- as.integer(df_str_final$ord)
  df_str_final_ord <<- df_str_final[order(df_str_final$ord),]
  df_str_final_ord <<- df_str_final_ord[df_str_final_ord$stre %in% val34,]
  df_str_final_ord <<- df_str_final_ord %>%  mutate(running_id = row_number())

  
  #### Insert into SQL database table | Insert into data.frame
  for (ii in 1:nrow(df_str_final_ord)){
    StrName <- as.character(df_str_final_ord$stre[ii])
    StrEnum <-as.integer(df_str_final_ord$running_id[ii])
    
   insert_to_dataframe(username,StrName,StrEnum)
   # insert_to_SQL(username, StrName, StrEnum) 
  }
  # Clean
  rm(df_str_final, df_str_final_ord, envir = .GlobalEnv)
}

##################
## Run the import
##################

UserArg <- 'Tomaz Kastrun'
filePathArg <- "/users/tomazkastrun/Documents/TomazKastrun_Clifton.pdf"

insert_PDF_first_page(UserArg, filePathArg)

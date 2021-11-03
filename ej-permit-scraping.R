

#Load required packages
library(pdftools)
library(tidyverse)
library(stringr)

#Setup
files <- list.files(pattern="pdf$")
permits <- lapply(files, pdf_text)

for (i in 1:length(permits)) {
  # List of character vectors for each page with no white space
  split <- strsplit(permits[[i]], "\\s+")
  
  #Each file is now a list of character strings
  permits[[i]] <- split %>% unlist
}

#List of pollutants. This will eventually add to the list of variables in our results table
pollutants <- c("VOC", "NOx", "NO2", "SOx", "Monoxide", "CO,", "NH3", "PM10", "PM2.5", "Benzene", "1,3-Butadiene", "Chromium", "Diesel PM", "Formaldehyde", "Hydrochloric", "HCL", "Hydrogen Phosphide", "Phosphine", "Gasoline", "Nickel")

# For reference, a large character object containing all the text in all the permits
permits_long <- unlist(permits)
permits_long <- str_c(permits_long[1:length(permits_long)], collapse=" ")

# Helper functions

#Extract Permit Unit from a permit
ex_unit <- function(permit) {
  if (str_detect(str_c(permit[1:length(permit)], collapse=" "), "UNIT:")) {
    index <- str_which(permit, "UNIT:")
    return(permit[index+1])
  }
  else {
    return(NA)
  }
}

#Extract expiration date from a permit
ex_expd <- function(permit) {
  if (str_detect(str_c(permit[1:length(permit)], collapse=" "), "DATE:")) {
    index <- str_which(permit, "DATE:")
    return(permit[index+1])
  }
  else {
    return(NA)
  }
}

#Extract equipment from a permit
ex_equip <- function(permit) {
  if (str_detect(str_c(permit[1:length(permit)], collapse=" "), "DESCRIPTION:")) {
    start_index <- str_which(permit, "DESCRIPTION:") + 1
    end_index <- str_which(permit, "REQUIREMENTS") - 3
    return(str_c(permit[start_index:end_index], collapse=" "))
  }
  else {
    return(NA)
  }
}

#Extract facility from a permit
ex_facility <- function(permit) {
  start_index <- str_which(permit, "Name:") + 1
  end_index <- str_which(permit, "Location:") - 1
  return(str_c(permit[start_index:end_index], collapse=" "))
}

#Extract Address from a permit
ex_address <- function(permit) {
  start_index <- str_which(permit, "Location:") + 1
  # permit_indices <- str_which(permit, ex_unit(permit)) - 1
  # v_indices <- lapply(permit_indices, function(x) x[x>start_index])
  end_index <- start_index
  while (str_sub(permit[end_index], start=1, end=2) !="N-") {
    end_index <- end_index+1
  }
  return(str_c(permit[start_index:(end_index-1)], collapse=" "))
}

#Grabs rules from a permit
ex_rules <- function(permit){
  #Setup
  rule_list <- c()
  i <- 1
  start <- i
  end <- i
  # Grabs raw rule list
  while (i < length(permit)) {
    if (str_sub(permit[i], start=1, end=1)=="[") {
      start <- i
    }
    if (str_detect(permit[i], "]")) {
      end <- i
      rule <- str_c(permit[start:end], collapse=" ")
      rule <- str_replace(rule, "\\[", "")
      rule <- str_replace(rule, "\\]", "")
      rule_list <- c(rule_list, rule)
      rule_list <- unique(rule_list)
    }
    i <- i+1
  }
  
  #Splits rule list into rules
  len <- length(rule_list)
  for (j in 1:len) {
    rule <- rule_list[j]
    rule <- str_replace(rule, "and ", "")
    if (str_detect(rule, ",")){
      sublist <- str_split(rule, ", ")[[1]]
      rule_list[j] <- str_replace(sublist[1], "Rules", "Rule")
      for (k in 2:length(sublist)) {
        rule_list <- c(rule_list, sublist[k])
      }
    }
    else{
      rule <- rule_list[j]
      if (str_detect(rule, "and")){
        sublist <- str_split(rule, " and ")[[1]]
        rule_list[j] <- str_replace(sublist[1], "Rules", "Rule")
        for (k in 2:length(sublist)) {
          rule_list <- c(rule_list, sublist[k])
        }
      }
    }
  }
  return(unique(rule_list))
}

# Check if a specific pollutant is mentioned in the permit
ex_pollutant <- function(permit, pollutant){
  s <- str_to_lower(pollutant)
  p <- str_to_lower(str_c(permit[1:length(permit)], collapse=" "))
  if (str_detect(p, s)) {
    return(TRUE)
  }
  else {
    return(FALSE)
  }
}


# Set up output table
results <- tibble(.rows=length(permits)) %>% mutate(Permit.Unit = "", Exp.Date="", Equip.Descrip="", Fac.Name="", Address="")
extra <- matrix(NA_real_, nrow=nrow(results), ncol=length(pollutants), dimnames=list(NULL, pollutants))
results <- dplyr::bind_cols(results, as.data.frame(extra))

# Add rules column that aggregates rules for each column
results <- mutate(results, every_rule="")

# Collect a list of all rules from all permits in the set
all_rules <- c()
for (k in 1:length(permits)) {
  all_rules <- unique(c(all_rules, ex_rules(permits[[k]])))
}

# Add columns for all rules in the dataset
extra2 <- matrix(NA_real_, nrow=nrow(results), ncol=length(all_rules), dimnames=list(NULL, all_rules))
results <- dplyr::bind_cols(results, as.data.frame(extra2))

# Fill in data values
for (i in 1:nrow(results)) {
  local_rules <- str_c(ex_rules(permits[[i]]), collapse=", ")
  results[i,1] <- ex_unit(permits[[i]])
  results[i,2] <- ex_expd(permits[[i]])
  results[i,3] <- ex_equip(permits[[i]])
  results[i,4] <- ex_facility(permits[[i]])
  results[i,5] <- ex_address(permits[[i]])
  
  #Add pollutant data
  for (j in 1:length(pollutants)){
    results[i, 5+j] <-  ex_pollutant(permits[[i]], pollutants[j])
  }
  
  #Adds value for 'every_rule' variable
  results[i, 26] <- local_rules
  
  # Add in all rule data
  n <- 1
  while (n < length(all_rules)){
    n_rule <- str_replace_all(all_rules[n], "\\(", "")
    n_rule <- str_replace_all(n_rule, "\\)", "")
    n_rule <- str_replace_all(n_rule, "\\{", "")
    n_rule <- str_replace_all(n_rule, "\\}", "")
    n_rule <- str_replace_all(n_rule, "\\[", "")
    n_rule <- str_replace_all(n_rule, "\\]", "")
    results[i, 6+j+n] <- str_detect(local_rules, n_rule)
    n <- n+1
  }
}


#Write out results
readr::write_csv(results, "Stockton AB 617 Facilities Permits.csv")



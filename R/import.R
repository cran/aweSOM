## Functions used by shiny app to read import data

import.csv.txt <- function(dataFile, header, sep, quote, 
                           dec, encoding, dataFile_datapath){
   if (is.null((dataFile))){ 
    return(NULL)
   }
  if (quote == "None") quote <- ""
  if (header %in% c("TRUE", "FALSE")) header <- as.logical(header)
  
  reprocode <- paste0(
    'import.data <- data.table::fread("', dataFile$name, '", ',
    if (header != "auto") paste0('header = "', header, '", '),
    if (sep != "auto") paste0('sep = "', sep, '", '),
    if (quote != "\"") paste0('quote = "', quote, '", '),
    if (dec != ".") paste0('dec = "', dec, '", '),
    if (encoding != "unknown") paste0('encoding = "', encoding, '", '),
    'stringsAsFactors = TRUE, data.table = FALSE)\n')
  
  data <- try(data.frame(data.table::fread(dataFile_datapath, 
                                           header = header, sep = sep, 
                                           quote = quote, dec = dec, 
                                           stringsAsFactors = TRUE,
                                           encoding = encoding)))
  if(class(data) == "try-error"){ return(NULL)}
  return(list(data, reprocode))
}


import.excel_xlsx <- function(dataFile, column_names, trim_spaces, range_specified_bol,
                                        range_specs, worksheet_specified_bol,  worksheet_specs,
                                        dataFile_datapath, rows_to_skip){
  if (is.null((dataFile))){ 
    return(NULL)
  }
  the.header <- switch(column_names, "TRUE"= TRUE, "FALSE" = FALSE)
  the.trim_spaces <- switch(trim_spaces, "TRUE"= TRUE, "FALSE" = FALSE)
  the.range <- NULL
  if(range_specified_bol == TRUE & range_specs != "") the.range <- range_specs
  the.sheet <- NULL
  if(worksheet_specified_bol == TRUE & worksheet_specs != "") the.sheet <- worksheet_specs
  
  reprocode <- paste0("import.data <- data.frame(readxl::read_xlsx('", 
                                   dataFile$name, "', ",
                                   "col_names = ", the.header, ", ",
                                   if (! is.null(the.range)) {
                                     paste0("range = '", the.range, "', ")
                                   },
                                   if (! is.null(the.sheet)) {
                                     paste0("sheet = '", the.sheet, "', ")
                                   },
                                   "trim_ws = ", the.trim_spaces, ", ",
                                   "skip = ", rows_to_skip,
                                   "))\n")
  
  data <- try(data.frame(readxl::read_xlsx(dataFile_datapath,
                                   col_names = the.header,
                                   range = the.range,
                                   sheet= the.sheet,
                                   trim_ws = the.trim_spaces,
                                   skip = rows_to_skip)))
  if(class(data) == "try-error") return(NULL)
  return(list(data, reprocode))
}


import.excel_xls <- function(dataFile, column_names_xls, trim_spaces_xls,
                             range_specified_bol_xls, range_specs_xls,
                             worksheet_specified_bol_xls, worksheet_specs_xls,
                             dataFile_datapath, rows_to_skip_xls){
  if (is.null((dataFile))){ 
    return(NULL)
  }
  the.header <- switch(column_names_xls, "TRUE"= TRUE, "FALSE" = FALSE)
  the.trim_spaces <- switch(trim_spaces_xls, "TRUE"= TRUE, "FALSE" = FALSE)
  the.range <- NULL
  if(range_specified_bol_xls == TRUE & range_specs_xls != "") the.range <- range_specs_xls
  the.sheet <- NULL
  if(worksheet_specified_bol_xls == TRUE & worksheet_specs_xls != "") the.sheet <- worksheet_specs_xls
  
  reprocode <- paste0("import.data <- data.frame(readxl::read_xls('", 
                                   dataFile$name, "', ",
                                   "col_names = ", the.header, ", ",
                                   if (! is.null(the.range)) {
                                     paste0("range = '", the.range, "', ")
                                   },
                                   if (! is.null(the.sheet)) {
                                     paste0("sheet = '", the.sheet, "', ")
                                   },
                                   "trim_ws = ", the.trim_spaces, ", ",
                                   "skip = ", rows_to_skip_xls,
                                   "))\n")

  data <- try(data.frame(readxl::read_xls(dataFile_datapath,
                                  col_names = the.header,
                                  range = the.range,
                                  sheet= the.sheet,
                                  trim_ws = the.trim_spaces,
                                  skip = rows_to_skip_xls)))
  if(class(data) == "try-error") return(NULL)
  return(list(data, reprocode))
}


import.spss <- function(dataFile, dataFile_datapath, skip_spss){
  if (is.null((dataFile))) return(NULL)

  reprocode <- paste0("import.data <- data.frame(haven::read_spss('", 
                                   dataFile$name, "', skip = ", 
                                   skip_spss, "))\n")
  
  data <- try(data.frame(haven::read_spss(file = dataFile_datapath, 
                                          skip= skip_spss))) 
  data <- data.frame(lapply(data, function(x) {
    attr(x, "format.spss") <- NULL
    if ("haven_labelled" %in% class(x)) 
      x <- haven::as_factor(x, levels= "labels")
    x
  }))
  if(class(data) == "try-error") return(NULL)
  return(list(data, reprocode))
}


import.stata <- function(dataFile, dataFile_datapath){
  if (is.null((dataFile))){ 
    return(NULL)
  }
  
  reprocode <- paste0("import.data <- data.frame(foreign::read.dta('", 
                                   dataFile$name, "'))\n")

  data <- try(data.frame(foreign::read.dta(file = dataFile_datapath)))
  if(class(data) == "try-error") return(NULL)
  return(list(data, reprocode))
}


import.sas.data <- function(dataFile, dataFile_datapath ){
  if (is.null((dataFile))) return(NULL)
  
  reprocode <- paste0("import.data <- data.frame(haven::read_sas('", 
                                   dataFile$name, "'))\n")
  
  data <- try(data.frame(haven::read_sas(data_file = dataFile_datapath)))
  if(class(data) == "try-error") return(NULL)
  
  return(list(data, reprocode))
}

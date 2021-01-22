

import.csv.txt <- function(input_dataFile, input_header, input_sep, input_quote, input_dec, input_encoding,
                                     input_dataFile_datapath){
   if (is.null((input_dataFile))){ 
    return(NULL)
  }
  
  the.header <- switch(input_header, "Auto"= "auto", 
                       "Header"= TRUE, "No Header"= FALSE)
  the.sep <- switch(input_sep, "Auto"= "auto", 
                    "Comma ','"=",", "Semicolon ';'"=";", 
                    "Tab"="\t", "Space"=" ")
  the.quote <- switch(input_quote, "None"= "",
                      "Double Quote \""= '"',
                      "Single Quote '"= "'")
  the.dec <- switch(input_dec, 'Period "."'=".", 'Comma ","'=",")
  the.encoding <- switch(input_encoding, "unknown" = "unknown", 
                         "UTF-8" = "UTF-8", "Latin-1" = "Latin-1")
  
  data_read_reproducible <- paste0("ok.data <- data.table::fread('", 
                                   input_dataFile$name, "', ",
                                    "header = '", the.header, 
                                    "', sep ='", the.sep, 
                                    "', quote = '", the.quote, 
                                    "', dec = '", the.dec, 
                                    "', stringsAsFactors = TRUE, encoding = '", 
                                   the.encoding, "', data.table = FALSE)\n")

  data <- try(data.frame(data.table::fread(input_dataFile_datapath, 
                                           header=the.header, sep=the.sep, 
                                           quote=the.quote, dec=the.dec, 
                                           stringsAsFactors=TRUE,
                                           encoding=the.encoding)))
  if(class(data) == "try-error"){ return(NULL)}
  return(list(data, data_read_reproducible))
}



import.excel_xlsx <- function(input_dataFile, input_column_names, input_trim_spaces, input_range_specified_bol,
                                        input_range_specs, input_worksheet_specified_bol,  input_worksheet_specs,
                                        input_dataFile_datapath, input_rows_to_skip){
  if (is.null((input_dataFile))){ 
    return(NULL)
  }
  the.header <- switch(input_column_names, "TRUE"= TRUE, "FALSE" = FALSE)
  the.trim_spaces <- switch(input_trim_spaces, "TRUE"= TRUE, "FALSE" = FALSE)
  the.range <- NULL
  if(input_range_specified_bol == TRUE & input_range_specs != "") the.range <- input_range_specs
  the.sheet <- NULL
  if(input_worksheet_specified_bol == TRUE & input_worksheet_specs != "") the.sheet <- input_worksheet_specs
  
  
  data_read_reproducible <- paste0("ok.data <- data.frame(readxl::read_xlsx('", 
                                   input_dataFile$name, "', ",
                                   "col_names = ", the.header, ", ",
                                   if (! is.null(the.range)) {
                                     paste0("range = '", the.range, "', ")
                                   },
                                   if (! is.null(the.sheet)) {
                                     paste0("sheet = '", the.sheet, "', ")
                                   },
                                   "trim_ws = ", the.trim_spaces, ", ",
                                   "skip = ", input_rows_to_skip,
                                   "))\n")
  
  data <- try(data.frame(readxl::read_xlsx(input_dataFile_datapath,
                                   col_names = the.header,
                                   range = the.range,
                                   sheet= the.sheet,
                                   trim_ws = the.trim_spaces,
                                   skip = input_rows_to_skip)))
  if(class(data) == "try-error") return(NULL)
  return(list(data, data_read_reproducible))
  
  
}



import.excel_xls <- function(input_dataFile, input_column_names_xls, input_trim_spaces_xls, input_range_specified_bol_xls,
                                       input_range_specs_xls, input_worksheet_specified_bol_xls, input_worksheet_specs_xls,
                                       input_dataFile_datapath, input_rows_to_skip_xls){
  if (is.null((input_dataFile))){ 
    return(NULL)
  }
  the.header <- switch(input_column_names_xls, "TRUE"= TRUE, "FALSE" = FALSE)
  the.trim_spaces <- switch(input_trim_spaces_xls, "TRUE"= TRUE, "FALSE" = FALSE)
  the.range <- NULL
  if(input_range_specified_bol_xls == TRUE & input_range_specs_xls != "") the.range <- input_range_specs_xls
  the.sheet <- NULL
  if(input_worksheet_specified_bol_xls == TRUE & input_worksheet_specs_xls != "") the.sheet <- input_worksheet_specs_xls
  
  
  data_read_reproducible <- paste0("ok.data <- data.frame(readxl::read_xls('", 
                                   input_dataFile$name, "', ",
                                   "col_names = ", the.header, ", ",
                                   if (! is.null(the.range)) {
                                     paste0("range = '", the.range, "', ")
                                   },
                                   if (! is.null(the.sheet)) {
                                     paste0("sheet = '", the.sheet, "', ")
                                   },
                                   "trim_ws = ", the.trim_spaces, ", ",
                                   "skip = ", input_rows_to_skip_xls,
                                   "))\n")
  
  
  
  
  data <- try(data.frame(readxl::read_xls(input_dataFile_datapath,
                                  col_names = the.header,
                                  range = the.range,
                                  sheet= the.sheet,
                                  trim_ws = the.trim_spaces,
                                  skip = input_rows_to_skip_xls)))
  if(class(data) == "try-error") return(NULL)
  return(list(data, data_read_reproducible))
}







import.spss <- function(input_dataFile, input_dataFile_datapath, input_skip_spss){
  if (is.null((input_dataFile))) return(NULL)

  data_read_reproducible <- paste0("ok.data <- data.frame(haven::read_spss('", 
                                   input_dataFile$name, "', skip = ", 
                                   input_skip_spss, "))\n")
  
  data <- try(data.frame(haven::read_spss(file = input_dataFile_datapath, 
                                          skip= input_skip_spss))) 
  data <- data.frame(lapply(data, function(x) {
    attr(x, "format.spss") <- NULL
    if ("haven_labelled" %in% class(x)) 
      x <- haven::as_factor(x, levels= "labels")
    x
  }))
  if(class(data) == "try-error") return(NULL)
  return(list(data, data_read_reproducible))
  
}



import.stata <- function(input_dataFile, input_dataFile_datapath){
  if (is.null((input_dataFile))){ 
    return(NULL)
  }
  
  data_read_reproducible <- paste0("ok.data <- data.frame(foreign::read.dta('", 
                                   input_dataFile$name, "'))\n")

  data <- try(data.frame(foreign::read.dta(file = input_dataFile_datapath)))
  if(class(data) == "try-error") return(NULL)
  return(list(data, data_read_reproducible))
}




import.sas.data <- function(input_dataFile, input_dataFile_datapath ){
  if (is.null((input_dataFile))) return(NULL)
  
  data_read_reproducible <- paste0("ok.data <- data.frame(haven::read_sas('", 
                                   input_dataFile$name, "'))\n")
  
  data <- try(data.frame(haven::read_sas(data_file = input_dataFile_datapath)))
  if(class(data) == "try-error") return(NULL)
  
  return(list(data, data_read_reproducible))
}



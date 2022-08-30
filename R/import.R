## Functions used by shiny app to read import data

import.csv.txt <- function(dataFile, header, sep, quote, dec, encoding){
  if (is.null(dataFile)) return(NULL)
  
  if (quote == "None") quote <- ""
  if (header %in% c("TRUE", "FALSE")) header <- as.logical(header)
  
  reprocode <- paste0(
    'import.data <- data.table::fread("', dataFile$name, '", ',
    if (header != "auto") paste0('header = "', header, '", '),
    if (sep != "auto") paste0('sep = "', sep, '", '),
    if (quote != "\"") paste0('quote = "', quote, '", '),
    if (dec != ".") paste0('dec = "', dec, '", '),
    if (encoding != "unknown") paste0('encoding = "', encoding, '", '),
    'stringsAsFactors = TRUE, data.table = FALSE, check.names = TRUE)\n')
  
  data <- try(data.table::fread(dataFile$datapath, header = header, sep = sep, 
                                quote = quote, dec = dec, 
                                stringsAsFactors = TRUE, encoding = encoding, 
                                data.table = FALSE, check.names = TRUE))
  if(inherits(data, "try-error")){ return(NULL)}
  return(list(data, reprocode))
}


import.ods <- function(dataFile, sheet, col_names, ods_na, skip, range) {
  if (is.null(dataFile)) return(NULL)
  
  if (range == "") range <- NULL
  reprocode <- paste0(
    'import.data <- readODS::read_ods("', dataFile$name, '"', 
    if (sheet != 1) paste0(', sheet = ', sheet),
    if (!col_names) ', col_names = FALSE',
    if (ods_na != '') paste0(', na = "', ods_na, '"'),
    if (skip > 0) paste0(', skip = ', skip), 
    if (!is.null(range)) paste0(', range = "', range, '"'), 
    ')\n')
  
  data <- try(readODS::read_ods(path = dataFile$datapath, sheet = sheet, 
                                col_names = col_names, na = ods_na, skip = skip,
                                range = range))
  if(inherits(data, "try-error")){ return(NULL)}
  return(list(data, reprocode))
}


import.excel <- function(dataFile, column_names, trim_spaces,
                         range_specified_bol, range_specs,
                         worksheet_specified_bol, worksheet_specs,
                         rows_to_skip){
  if (is.null(dataFile)) return(NULL)
  
  column_names <- as.logical(column_names)
  trim_spaces <- as.logical(trim_spaces)
  the.range <- NULL
  if(range_specified_bol == TRUE & range_specs != "") the.range <- range_specs
  the.sheet <- NULL
  if(worksheet_specified_bol == TRUE & worksheet_specs != "") 
    the.sheet <- worksheet_specs
  
  reprocode <- paste0('import.data <- data.frame(readxl::read_excel("', 
                      dataFile$name, '", ',
                      'col_names = ', column_names, ', ',
                      if (! is.null(the.range)) {
                        paste0('range = "', the.range, '", ')
                      },
                      if (! is.null(the.sheet)) {
                        paste0('sheet = "', the.sheet, '", ')
                      },
                      if (!trim_spaces) 'trim_ws = FALSE', 
                      if (rows_to_skip > 0)
                        paste0('skip = ', rows_to_skip),
                      '))\n')
  
  data <- try(data.frame(readxl::read_excel(dataFile$datapath,
                                            col_names = column_names,
                                            range = the.range,
                                            sheet= the.sheet,
                                            trim_ws = trim_spaces,
                                            skip = rows_to_skip)))
  if(inherits(data, "try-error")) return(NULL)
  return(list(data, reprocode))
}


import.spss <- function(dataFile, skip, user_na){
  if (is.null(dataFile)) return(NULL)

  reprocode <- paste0(
    'import.data <- data.frame(haven::read_spss("', dataFile$name, '"',
    if (skip > 0)
      paste0(', skip = ', skip), 
    if (user_na) 
      paste0(', user_na = ', user_na), 
    '))\n',
    'import.data <- data.frame(lapply(import.data, function(x) {\n', 
    '  attr(x, "format.spss") <- NULL\n',
    '  if ("haven_labelled" %in% class(x))\n', 
    '  x <- haven::as_factor(x, levels= "labels")\n',
    '  x\n',
    '}))\n')
  
  data <- try(data.frame(haven::read_spss(file = dataFile$datapath, 
                                          skip= skip, user_na = user_na))) 
  data <- data.frame(lapply(data, function(x) {
    attr(x, "format.spss") <- NULL
    if ("haven_labelled" %in% class(x)) 
      x <- haven::as_factor(x, levels= "labels")
    x
  }))
  if(inherits(data, "try-error")) return(NULL)
  return(list(data, reprocode))
}

import.sas.data <- function(dataFile, catalog_file, skip){
  if (is.null(dataFile)) return(NULL)
  
  reprocode <- paste0(
    'import.data <- data.frame(haven::read_sas("', dataFile$name, '"', 
    if (!is.null(catalog_file$name)) 
      paste0(', catalog_file = "', catalog_file$name ,'"'),
    if (skip > 0)
      paste0(', skip = ', skip),
    '))\n')
  
  data <- try(data.frame(haven::read_sas(data_file = dataFile$datapath, 
                                         catalog_file = catalog_file$datapath, 
                                         skip = skip)))
  if(inherits(data, "try-error")) return(NULL)
  
  return(list(data, reprocode))
}

import.stata <- function(dataFile, convert_factors){
  if (is.null(dataFile)) return(NULL)
  
  reprocode <- paste0('import.data <- data.frame(foreign::read.dta("', 
                      dataFile$name, '", convert.factors = ', convert_factors, 
                      '))\n')

  data <- try(data.frame(foreign::read.dta(file = dataFile$datapath)))
  if(inherits(data, "try-error")) return(NULL)
  return(list(data, reprocode))
}

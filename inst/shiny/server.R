################################################################################
## Global Variables
################################################################################

## List of possible plots, by "what" type
plotChoices <- list(MapInfo= c("Observations Cloud"= "Cloud",
                               "Population map"= "Hitmap",
                               "Superclass Dendrogram"= "Dendrogram",
                               "Superclass Scree plot"= "Screeplot",
                               "Superclass Silhouette"= "Silhouette",
                               "Neighbour distance"= "UMatrix", 
                               "Smooth distance"= "SmoothDist"), 
                    Numeric= c("Circular Barplot"= "Circular", 
                               "Barplot"= "Barplot", 
                               "Boxplot"= "Boxplot",
                               "Line plot"= "Line", 
                               "Radar chart"= "Radar",
                               "Heat map (Color)"= "Color"), 
                    Categorical= c("Pie"= "Pie", 
                                   "Barplot"= "CatBarplot"))


help_messages <- list(
  filesize = HTML("<p>The input data should be tabular, with rows representing observations and columns representing variables.</p>
                   <p>By default, Shiny limits file uploads to 5MB per file. You can modify this limit by using the `shiny.maxRequestSize` option. For example, adding `options(shiny.maxRequestSize = 30*1024^2)` before running `aweSOM()` would increase the limit to 30MB.</p>"),
  import_data_panel = HTML("<h3>Working with aweSOM</h3> <br>
                          Use this interface to train and visualize self-organizing maps (SOM, aka Kohonen maps).
                          Use the tabs above in sequence : <br>
                          <strong>Import Data:</strong> Import the data to analyze<br>
                          <strong>Train:</strong> Train the SOM on selected variables<br>
                          <strong>Plot:</strong> Visualize the trained SOM <br>
                          <strong>Export Data:</strong> Export the trained SOM or clustered data <br>
                          <strong>R Script:</strong> Generate the R script to reproduce your analysis in R <br>
                          <strong>About:</strong> Further information on this application <br>"),
  help_scale = HTML("<h3>Scale training data</h3>
                    <p> It is recommended to scale the variables before training, so that each variable has the same weight in the total variance.</p>
                    <p> For numeric variables, scaling to 0 mean and unit variance is performed. For categorical variables, each dummy-transformed category is divided by the square root of its mean (the same treatment as in MCA).</p>
                    <p> The user-set variables weights are applied after scaling (or on unscaled data if scaling is not chosen).</p>"),
  help_seed = HTML("<h3>Set pseudo-random seed</h3>
                   <p>Seed of the pseudo-random number generator. 
                   This allows the results to be reproduced in later work.</p>"),
  help_init = HTML("<h3> SOM initialization </h3>
                   <p> Method for prototype initialization. </p>
                   <p> 'PCA Obs' takes as prototypes the observations that are closest to 
                   the nodes of a 2d grid placed along the first two components of a PCA.</p>
                   <p> The 'PCA' method uses the nodes instead of the observations.</p>
                   <p> The 'Random Obs' method samples random observations.</p>"),
  help_maxNA = HTML("<h3> Maximum allowed NAs per observation </h3>
                    <p> Missing values (NA) are natively handled by SOM. </p>
                    <p> This parameter controls the fraction of NA allowed in any 
                    observation. Observations with more NA than this fraction will be removed.</p>
                    <p> When dummy-encoded categorical variables are present, 
                    this fraction applies to the sum of chosen levels of each variable.</p>"),
  help_rlen = HTML("<h3>Advanced Training Options</h3>
                   <p><strong>Rlen:</strong>  Number of times the complete data set will be presented to the network. </p>
                   <p><strong>Alpha:</strong> Learning rate. </p>
                   <p><strong>Radius:</strong> Neighborhood Radius. </p>
                   <p> For more information, refer to the documentation of the kohonen package. </p>"),
  help_weights = HTML("<h3>Weights</h3>
                      <p> The weights determine how much each variable contributes to the total variance, and thus influences the layout of the map.</p>
                      <p> If the variables are scaled, and all weights are 1, then all numeric variables contribute equally to the total variance, while each categorical variable contributes (nb_categories-1) times more than a numeric variable.</p>
                      <p> The 'balance weights' button automatically sets the weights to compensates this effect, so that all variables, categorical or numeric, contribute equally.</p>
                      <p> In practice, each variable is multiplied by the square root of its weight before training.</p>"),
  help_types = HTML("<h3>Variables types</h3>
                    <p> Two types of variables are supported in aweSOM: numeric and factor (categorical). The dropdown type selectors allows the type of each variable to be changed.</p>
                    <p> When converting a factor to numeric, all levels whose labels are not numbers will become NA.</p>
                    <p> When converting a numeric to factor, each value taken by the variable becomes a distinct level. This should typically not be done with a large number of levels. </p>
                    <p> The weights that can be adjusted for each variable are explained in the 'weights' help message in the training panel.</p>"),
  help_levels = HTML("<h3> Levels</h3>
                     <p> Categorical variables (factors) are dummy-encoded before they are processed by SOM. Each level becomes a separate 0-1 variable in the training data. </p>
                     <p> As in specific MCA, it is possible to remove levels from the training data, by unselecting them in the levels box. This is typically done for categories that are not informative for the question at hand.</p>"),
  help_contrast = HTML("<h3>Variables scales</h3>
                       <p> All values that are used for the plots (means, medians, prototypes) are scaled to 0-1 for display (minimum height and maximum height). This parameter controls how this scaling is done. </p>
                       <p> <strong>Contrast:</strong> for each variable, the minimum height is the minimum observed mean/median/prototype on the map, the maximum height is the maximum on the map. This ensures maximal contrast on the plot. </p> 
                       <p> <strong>Observations Range:</strong> for each variable, the minimum height corresponds to the minimum of that variable over the whole dataset, the maximum height to the maximum of the variable on the whole dataset.</p>
                       <p> <strong>Same Scales:</strong> all heights are displayed on the same scale, using the global minimum and maximum of the dataset.</p>"),
  help_average_format =  HTML("<h3>Values</h3> <br> What value to display <br>
                       <strong>Observation Means:</strong> Means of observations per cell <br>
                       <strong>Observation Medians:</strong> Medians of observations per cell <br>
                       <strong>Prototypes:</strong> Prototype values per cell <br>"), 
  help_cloud = HTML("<h3>Observations cloud</h3>
                    <p>See help of aweSOMplot for more details on how each method computes the positions.
                    <br/> <strong> Cell-wise PCA:</strong> PCA on the training data of each cell, separately.
                    <br/> <strong> Cell-centered kPCA: </strong> kernel PCA performed on all the differences between the training data and their cell's prototype.
                    <br/> <strong> Cell-centered PCA: </strong> Similar to cell-centered kPCA, but with PCA instead of kernel PCA.
                    <br/> <strong> Prototype Proximity:</strong> Points close to their cell's center are close to their closest prototype, while points close to another cell are close to that cell's prototype.
                    <br/> <strong> Random: </strong>the point coordinates are random samples from a uniform distribution.
                    </p>")
)


################################################################################
## Main server function
################################################################################

shinyServer(function(input, output, session) {
  
  values <- reactiveValues()
  
  #############################################################################
  ## Events for help messages
  #############################################################################
  
  observeEvent(input$help_filesize, {
    showNotification(help_messages$filesize, type = "message", duration = 60) 
  })
  observeEvent(input$help_message_intro_to_aweSOM, {
    showNotification(help_messages$import_data_panel, type = "message", duration = 60) 
  })
  observeEvent(input$help_scale, {
    showNotification(help_messages$help_scale, type = "message", duration = 60) 
  })
  observeEvent(input$help_seed, {
    showNotification(help_messages$help_seed, type = "message", duration = 60) 
  })
  observeEvent(input$help_init, {
    showNotification(help_messages$help_init, type = "message", duration = 60) 
  })
  observeEvent(input$help_maxNA, {
    showNotification(help_messages$help_maxNA, type = "message", duration = 60) 
  })
  observeEvent(input$help_rlen, {
    showNotification(help_messages$help_rlen, type = "message", duration = 60) 
  })
  observeEvent(input$help_weights, {
    showNotification(help_messages$help_weights, type = "message", duration = 60) 
  })
  observeEvent(input$help_types, {
    showNotification(help_messages$help_types, type = "message", duration = 60) 
  })
  observeEvent(input$help_levels, {
    showNotification(help_messages$help_levels, type = "message", duration = 60) 
  })
  observeEvent(input$help_contrast, {
    showNotification(help_messages$help_contrast, type = "message", duration = 60) 
  })
  observeEvent(input$help_average_format, {
    showNotification(help_messages$help_average_format, type = "message", duration = 60) 
  })
  observeEvent(input$help_cloud, {
    showNotification(help_messages$help_cloud, type = "message", duration = 60) 
  })
  
  
  #############################################################################
  ## Panel "Import Data"
  #############################################################################

  ## Current imported data
  ok.data <- reactive({
    if(input$file_type == "csv_txt"){ 
      imported_file_object <- aweSOM:::import.csv.txt(
        dataFile = input$dataFile, header = input$header, 
        sep = input$sep, quote = input$quote, dec = input$dec,
        encoding = input$encoding)
    } else if (input$file_type == "ods") {
      imported_file_object <- aweSOM:::import.ods(
        dataFile = input$dataFile, 
        sheet = input$ods_sheet, 
        col_names = input$ods_col_names, 
        ods_na = input$ods_na, 
        skip = input$ods_skip, 
        range = input$ods_range)
    } else if(input$file_type == "excel"){
      imported_file_object <-  aweSOM:::import.excel(
        dataFile = input$dataFile, 
        column_names = input$column_names, 
        trim_spaces = input$trim_spaces, 
        range_specified_bol = input$range_specified_bol, 
        range_specs = input$range_specs,
        worksheet_specified_bol = input$worksheet_specified_bol,
        worksheet_specs = input$worksheet_specs, 
        rows_to_skip = input$rows_to_skip)
    } else if(input$file_type == "spss"){
      imported_file_object <- aweSOM:::import.spss(
        dataFile = input$dataFile, 
        skip = input$skip_spss,
        user_na = input$user_na_spss)
    } else if(input$file_type == "sas_data"){
      imported_file_object <- aweSOM:::import.sas.data(
        dataFile = input$dataFile, 
        catalog_file = if (input$sas_use_catalog) input$sas_catalog_file else NULL,
        skip = input$sas_skip)
    } else if(input$file_type == "stata"){
      imported_file_object <- aweSOM:::import.stata(
        dataFile = input$dataFile, 
        convert_factors = input$convert_factors_stata)
    } 
    
    isolate({
      values$codetxt$dataread <- 
        paste0('## Import Data\n', 
               '# setwd("/path/to/datafile/directory") ## Uncomment this line and set the path to the datafile\'s directory\n',
               imported_file_object[[2]])
    })
    imported_file_object[[1]]
  })
  
  
  ## data preview table
  output$dataView <- DT::renderDataTable({
    d.input <- ok.data()
    if (is.null(d.input)) return(NULL)
    d.input
  })


  ## data import message
  output$dataImportMessage <- renderUI({
    if (is.null(input$dataFile)) 
      return(HTML("<h4 style='text-align: center;'> Welcome to aweSOM! </h4>
                  <h4 style='text-align: center;'> Import data to get started. </h4>"))
    if (! is.null(input$dataFile) & is.null(ok.data())) 
      return(HTML("<h4 style='text-align: center;'>Data import failed.</h4> <h5 style='text-align: center;'>Check that the specified file type is correct, or try different import parameters.</h5>"))
    HTML("<h4 style='text-align: center;'> Data import successful, proceed to Train.</h4><br/>")
  })
  
  #############################################################################
  ## Panel "Train"
  #############################################################################
  
  ## Update grid dimension on data update
  observe({
    if (is.null(ok.data())) return()
    tmp.dim <- max(4, min(10, ceiling(sqrt(nrow(ok.data()) / 10))))
    updateNumericInput(session, "kohDimx", value= tmp.dim)
    updateNumericInput(session, "kohDimy", value= tmp.dim)
  })
  
  ## Update training radius on change of grid
  observe({
    if (is.null(ok.data())) return()
    tmpgrid <- kohonen::somgrid(input$kohDimx, input$kohDimy, input$kohTopo)
    tmpgrid$n.hood <- ifelse(input$kohTopo == "hexagonal", "circular", "square")
    radius <- round(unname(quantile(kohonen::unit.distances(tmpgrid, FALSE), .67)), 2)
    updateNumericInput(session, "trainRadius1", value= radius)
    updateNumericInput(session, "trainRadius2", value= -radius)
  })
  
  output$trainVarSelect <- renderUI({
    if (is.null(ok.data())) {
      selectInput("trainVarChoice", "", 
                  choices = "Variables will appear here", 
                  selected = "Variables will appear here", 
                  multiple = TRUE, selectize = FALSE)
    } else {
      selectVars <- sapply(ok.data(), class) %in% c("integer", "numeric")
      selectInput("trainVarChoice", "", width = "100%",
                  choices = colnames(ok.data()), 
                  selected = colnames(ok.data())[selectVars], 
                  multiple = TRUE, selectize = FALSE, 
                  size = min(50, max(10, ncol(ok.data()))))
    }
  })

  ## Update train variable choices on button click
  observeEvent(input$varNum, {
    if (is.null(ok.data())) return()
    selectVars <- sapply(ok.data(), class) %in% c("integer", "numeric")
    updateSelectInput(session, "trainVarChoice", choices = colnames(ok.data()), 
                      selected = colnames(ok.data())[selectVars])
  })
  observeEvent(input$varAll, {
    if (is.null(ok.data())) return()
    updateSelectInput(session, "trainVarChoice", choices = colnames(ok.data()), 
                      selected = colnames(ok.data()))
  })
  observeEvent(input$varNone, {
    if (is.null(ok.data())) return()
    updateSelectInput(session, "trainVarChoice", choices = colnames(ok.data()), 
                      selected = NULL)
  })
  
  ## Train variables weights and type
  output$trainVarOptions <-renderUI({
    if (is.null(ok.data())) return()
    varclass <- sapply(ok.data(), class)
    if (is.null(varclass)) return()
    isolate({
      if (is.null(values$trainVarOptionsFlag)) {
        values$trainVarOptionsFlag <- 1
      } else values$trainVarOptionsFlag <- values$trainVarOptionsFlag + 1
    })
    isnum <- varclass %in% c("integer", "numeric")
    names(isnum) <- names(varclass) <- colnames(ok.data())
    
    lapply(colnames(ok.data()), function(var) {
      conditionalPanel(
        paste0("input.trainVarChoice.includes('", var, "')"),
        fluidRow(column(4, selectInput(paste0("trainVarType", 
                                              gsub("[.]", "_", var)), 
                                       NULL, selectize = FALSE,
                                       choices = c("numeric", "factor"), 
                                       selected = ifelse(isnum[var], 
                                                         "numeric", "factor"))), 
                 column(3, numericInput(paste0("trainVarWeight", var), NULL, 
                                        value= 1, min= 0, step= 1e-3)), 
                 column(5, p(var))),
        conditionalPanel(
          paste0("input.trainVarType", gsub("[.]", "_", var), "== 'factor'"), 
          fluidRow(column(1), 
                   column(11, selectInput(paste0("trainVarLevels", var), NULL,
                                          choices = "...", selectize = FALSE,
                                          multiple= TRUE)))))
    })
  })
  
  ## Populate training factors' levels only when asked to
  trainVarTypeReac <- reactive({
    lapply(colnames(ok.data()), function(ivar) 
      input[[paste0("trainVarType", gsub("[.]", "_", ivar))]])
  })
  observeEvent(list(input$trainVarChoice, trainVarTypeReac()), {
    if (is.null(ok.data())) return()
    if (is.null(values$trainVarLevelsLoaded) |
        !isTRUE(all.equal(names(values$trainVarLevelsLoaded), 
                          colnames(ok.data())))) {
      values$trainVarLevelsLoaded <- rep(FALSE, ncol(ok.data()))
      names(values$trainVarLevelsLoaded) <- colnames(ok.data())
    }
    if (all(input$trainVarChoice %in% names(values$trainVarLevelsLoaded))) {
      for (ivar in input$trainVarChoice) {
        if (! is.null(input[[paste0("trainVarType", 
                                    gsub("[.]", "_", ivar))]])) {
          if (input[[paste0("trainVarType", 
                            gsub("[.]", "_", ivar))]] == "factor") {
            if (!values$trainVarLevelsLoaded[[ivar]]) {
              values$trainVarLevelsLoaded[[ivar]] <- TRUE
              updateSelectInput(session, paste0("trainVarLevels", ivar), 
                                choices = levels(as.factor(ok.data()[, ivar])), 
                                selected = levels(as.factor(ok.data()[, ivar])))
            }
          }
        }
      }
    }
  })
  
  ## Update train weights on button click
  observeEvent(input$weightsToOne, {
    if (is.null(ok.data())) return()
    for (ivar in input$trainVarChoice) {
      updateNumericInput(session, paste0("trainVarWeight", ivar), value= 1)
    }
  })
  observeEvent(input$weightsToComp, {
    if (is.null(ok.data())) return()
    for (ivar in input$trainVarChoice) {
      if (input[[paste0("trainVarType", gsub("[.]", "_", ivar))]] == "numeric") {
        newWeight <- 1
      } else {
        chosenlevels <- input[[paste0("trainVarLevels", ivar)]]
        if (!length(chosenlevels)) { 
          newWeight <- 1 
        } else if (length(chosenlevels) == 1) {
          newWeight <- 1 / (1 - mean(ok.data()[, ivar] == chosenlevels, na.rm = TRUE))
        } else { 
          newWeight <- 1 / (length(chosenlevels) - 1)
        }
      }
      newWeight <- round(newWeight, 3)
      updateNumericInput(session, paste0("trainVarWeight", ivar), value= newWeight)
    }
  })
  
  
  ## Create training data when button is hit
  ok.traindat <- reactive({
    if (input$trainbutton == 0) return(NULL)
    input$retrainButton
    
    isolate({
      if (is.null(ok.data())) return(NULL)
      if (input$kohDimx * input$kohDimy < 2) 
        return(list(dat= NULL, msg= "Map too small: must have at least 2 cells (add rows or columns)."))
      
      err.msg <- NULL
      codeTxt <- list()
      
      varWeights <- sapply(paste0("trainVarWeight", colnames(ok.data())),
                           function(var) ifelse(length(input[[var]]), input[[var]], 0))
      varSelected <- varWeights > 0 & colnames(ok.data()) %in% input$trainVarChoice
      if (sum(varSelected) < 2)
        return(list(dat= NULL, msg= "Select at least two variables (with non-zero weight)."))
      

      ## Keep only selected vars
      dat <- ok.data()[, varSelected]
      varWeights <- varWeights[varSelected]
      varNumeric <- sapply(dat, is.numeric)

      ## Change type if necessary, and save those varnames in reactive values
      varNumToFact <- varNumeric & sapply(colnames(dat), function(x) 
        input[[paste0("trainVarType", gsub("[.]", "_", x))]] == "factor")
      varFactToNum <- (!varNumeric) & sapply(colnames(dat), function(x) 
        input[[paste0("trainVarType", gsub("[.]", "_", x))]] == "numeric")
      for (ivar in colnames(dat)[varNumToFact]) {
        dat[, ivar] <- as.factor(dat[, ivar])
        codeTxt$NumToFact <- paste0(
          codeTxt$NumToFact, 
          'import.data[, "', ivar, '"] <- as.factor(import.data[, "', ivar, '"])\n')
      }
      for (ivar in colnames(dat)[varFactToNum]) {
        dat[, ivar] <- suppressWarnings(as.numeric(as.character(dat[, ivar])))
        codeTxt$FactToNum <- paste0(
          codeTxt$FactToNum, 
          'import.data[, "', ivar, '"] <- as.numeric(as.character(import.data[, "', ivar, '"]))\n')
      }
      values$varNumToFact <- colnames(dat)[varNumToFact]
      values$varFactToNum <- colnames(dat)[varFactToNum]
      varNumeric <- sapply(dat, is.numeric)
      

      varMode <- "mixed"
      if (!any(varNumeric)) varMode <- "categorical"
      if (!any(!varNumeric)) varMode <- "numeric"
      
      ## Transform non-numeric variables to dummies, excluding dropped levels
      if (varMode %in% c("categorical", "mixed")) {
        datQual <- aweSOM::cdt(dat[!varNumeric])
        varWeightsQual <- rep(varWeights[!varNumeric], 
                              times= sapply(dat[!varNumeric], function(x) nlevels(as.factor(x))))
        varLevels <- do.call(c, lapply(colnames(dat)[!varNumeric], 
                                       function(x) paste0(x, "_", input[[paste0("trainVarLevels", x)]])))
        droppedLevels <- colnames(datQual)[! colnames(datQual) %in% varLevels]
        varWeightsQual <- varWeightsQual[colnames(datQual) %in% varLevels]
        datQual <- as.matrix(datQual[, colnames(datQual) %in% varLevels, drop = FALSE])
        datNum <- as.matrix(dat[, varNumeric, drop = FALSE])
        varWeights <- varWeights[varNumeric]
      } else varWeightsQual <- NULL
      
      codeTxt$sel <- switch(
        varMode,
        numeric= paste0(
          '\n## Build training data (numeric)\n',
          'train.data <- import.data[, c("',
          paste(colnames(dat), collapse= '", "'), '")]\n', 
          if (any(varWeights != 1)) {
            paste0("varWeights <- c(",
                   paste0(colnames(dat), " = ", varWeights, collapse= ", "), ")\n")
          }
        ), 
        categorical= paste0(
          '\n## Build training data (categorical to dummies)\n',
          'cat.data <- import.data[c("', paste(colnames(dat), collapse= '", "'), 
          '")]\n',
          'train.data <- cdt(cat.data)\n',
          if (length(droppedLevels) > 0) {
            paste0("### Drop unselected factor levels\n", 
                   'train.data <- train.data[, ! colnames(train.data) %in% c("', 
                   paste0(droppedLevels, collapse= '", "'), '")]\n')
          },
          'catLevels <- colnames(train.data)\n',
          if (any(varWeightsQual != 1)) {
            paste0('varWeights <- c("', 
                   paste0(varLevels, '" = ', varWeightsQual, collapse= ', "'), ')\n')
          }
        ), 
        mixed= paste0(
          '\n## Build training data (mixed, categorical to dummies)\n',
          'numVars <- c("', paste(colnames(dat)[varNumeric], collapse= '", "'), '")\n',
          'catVars <- c("', paste(colnames(dat)[!varNumeric], collapse= '", "'), '")\n',
          'cat.data <- cdt(import.data[catVars])\n',
          if (length(droppedLevels) > 0) {
            paste0("### Drop unselected factor levels\n", 
                   'cat.data <- cat.data[, ! colnames(cat.data) %in% c("', 
                   paste0(droppedLevels, collapse= '", "'), '"), drop = FALSE]\n')
          },
          'catLevels <- colnames(cat.data)\n',
          'train.data <- as.matrix(cbind(import.data[numVars], cat.data))\n',
          if (any(c(varWeights, varWeightsQual) != 1)) {
            paste0('varWeights <- c("',
                   paste0(colnames(datNum), '" = ', varWeights, collapse= ', "'), ', "',
                   paste0(varLevels, '" = ', varWeightsQual, collapse= ', "'), ')\n')
          }
        )
      )
      
      ## Remove rows with too many NA in training variables
      if (varMode == "numeric") {
        NArows <- rowSums(is.na(dat)) > input$trainMaxNA * ncol(dat)
        trainMaxNAfloor <- floor(input$trainMaxNA * ncol(dat))
        dat <- dat[!NArows, ]
      } else {
        NArows <- rowSums(is.na(cbind(datNum, datQual))) > 
          input$trainMaxNA * (ncol(datNum) + ncol(datQual))
        trainMaxNAfloor <- floor(input$trainMaxNA * (ncol(datNum) + ncol(datQual)))
        datNum <- datNum[!NArows, , drop = FALSE]
        datQual <-datQual[!NArows, , drop = FALSE]
      }
      if (all(NArows)) {
        err.msg$NArows <- "All observations contain too many missing values, training impossible."
        return(list(dat= NULL, msg= err.msg))
      }
      if (any(NArows)) {
        err.msg$NArows <- paste(
          sum(NArows), "observations contained too many missing values and were removed.")
        codeTxt$NArows <- paste0(
          'NArows <- rowSums(is.na(train.data)) > ', trainMaxNAfloor, '\n', 
          'train.data <- train.data[!NArows, ]\n')
      }
      
      ## Check for constant variables (if so, exclude and message)
      if (varMode != "numeric") {
        varConstant <- apply(datNum, 2, function(x) 
          all(x == x[which(!is.na(x))[1]], na.rm= TRUE))
        varConstantQual <- apply(datQual, 2, function(x) 
          all(x == x[which(!is.na(x))[1]], na.rm= TRUE))
      } 
      if (varMode == "numeric") {
        varConstant <- apply(dat, 2, function(x) 
          all(x == x[which(!is.na(x))[1]], na.rm= TRUE))
        if (any(varConstant)) {
          if (sum(!varConstant) < 2) {
            err.msg$allconstant <- 
              'Less than two selected non-constant variables, training impossible.'
            return(list(dat= NULL, msg= err.msg))
          }
          err.msg$constant <- paste0(
            'Variables < ', paste(colnames(dat)[varConstant], collapse= ', '),
            ' > are constant, and will be removed for training.')
          codeTxt$constant <- paste0(
            'varConstant <- colnames(train.data) %in% c("', 
            paste(colnames(dat)[varConstant], collapse= '", "'), '")\n', 
            'train.data <- train.data[, !varConstant]\n', 
            if (any(varWeights != 1)) paste0('varWeights <- varWeights[!varConstant]\n'))
          dat <- dat[, !varConstant]
          varWeights <- varWeights[!varConstant]
          varNumeric <- varNumeric[!varConstant]
        }
      } else if (varMode == "categorical") {
        if (any(varConstantQual)) {
          if (sum(!varConstantQual) < 2) {
            err.msg$allconstant <- 
              'Less than two selected non-constant variables, training impossible.'
            return(list(dat= NULL, msg= err.msg))
          }
          err.msg$constant <- paste0(
            'Variables < ', paste(colnames(datQual)[varConstantQual], collapse= ', '),
            ' > are constant, and will be removed for training.')
          codeTxt$constant <- paste0(
            'varConstant <- colnames(train.data) %in% c("', 
            paste(colnames(datQual)[varConstantQual], collapse= '", "'), '")\n', 
            'train.data <- train.data[, !varConstant]\n', 
            if (any(varWeights != 1)) paste0('varWeights <- varWeights[!varConstant]\n'))
          datQual <- datQual[, !varConstantQual]
          varLevels <- varLevels[!varConstantQual]
          varWeightsQual <- varWeightsQual[!varConstantQual]
        }
      } else if (varMode == "mixed") {
        if (any(c(varConstant, varConstantQual))) {
          if (sum(!varConstant) + sum(!varConstantQual) < 2) {
            err.msg$allconstant <- 
              'Less than two selected non-constant variables, training impossible.'
            return(list(dat= NULL, msg= err.msg))
          }
          err.msg$constant <- paste0(
            'Variables < ', 
            paste(c(colnames(dat)[varConstant], colnames(datQual)[varConstantQual]), 
                  collapse= ', '), 
            ' > are constant, and will be removed for training.')
          codeTxt$constant <- paste0(
            'varConstant <- colnames(train.data) %in% c("',
            paste(c(colnames(datNum)[varConstant], colnames(datQual)[varConstantQual]), 
                  collapse= ', '), '")\n', 
            'train.data <- train.data[, !varConstant]\n', 
            if (any(c(varWeights, varWeightsQual) != 1)) paste0(
              'varWeights <- varWeights[!varConstant]\n'), 
            if (any(!varConstant)) paste0(
              'numVars <- numVars[! numVars %in% c("', 
              paste(colnames(datNum)[varConstant], collapse= '", "'), '")]\n'
            ),
            if (any(!varConstantQual)) paste0(
              'catLevels <- catLevels[! catLevels %in% c("', 
              paste(colnames(datQual)[varConstantQual], collapse= '", "'), '")]\n'
            ))
          datNum <- datNum[, !varConstant]
          varWeights <- varWeights[!varConstant]
          varNumeric <- varNumeric[!varConstant]
          datQual <- datQual[, !varConstantQual]
          varWeightsQual <- varWeightsQual[!varConstantQual]
          varLevels <- varLevels[!varConstantQual]
        }
      }

      ## Scale variables and apply weights
      if (varMode == "numeric") {
        if (input$trainscale) dat <- scale(dat)
        dat <- t(t(dat) * sqrt(varWeights))
      } else if (varMode == "categorical") {
        if (input$trainscale) 
          datQual <- t(t(datQual) / sqrt(colMeans(datQual, na.rm = TRUE)))
        datQual <- t(t(datQual) * sqrt(varWeightsQual))
        dat <- datQual
      } else if (varMode == "mixed") {
        if (input$trainscale) {
          datNum <- scale(datNum)
          datQual <- t(t(datQual) / sqrt(colMeans(datQual, na.rm = TRUE)))
        }
        datNum <- t(t(datNum) * sqrt(varWeights))
        datQual <- t(t(datQual) * sqrt(varWeightsQual))
        dat <- cbind(datNum, datQual)
      }

      if (input$trainscale) {
        codeTxt$scale <- switch(
          varMode, 
          numeric= paste0(
            '### Scale training variables (unit variance)\n',
            'train.data <- scale(train.data)\n'), 
          categorical= paste0(
            '### Scale training variables (MCA-type scaling)\n',
            'train.data <- t(t(train.data) / sqrt(colMeans(train.data, na.rm = TRUE)))\n'), 
          mixed= paste0(
            '### Scale training variables (numeric: unit variance, categorical: MCA-type)\n', 
            'train.data[, numVars] <- scale(train.data[, numVars])\n',
            'train.data[, catLevels] <- \n', 
            '  t(t(train.data[, catLevels]) / sqrt(colMeans(train.data[, catLevels, drop = FALSE], na.rm = TRUE)))\n')
          )
      } else if (varMode != "categorical") 
        codeTxt$scale <- "train.data <- as.matrix(train.data)\n"
      
      if (any(c(varWeights, varWeightsQual) != 1)) {
        codeTxt$scale <- paste0(
          codeTxt$scale, 
          '### Apply variables weights\n',
          'train.data <- t(t(train.data) * sqrt(varWeights))\n')
      }

      ## Prepare plot data (repro code)
      codeTxt$join <- paste0(
        '### Prepare plotting data\n', 
        switch(
          varMode,
          numeric= paste0('plot.data <- import.data'), 
          categorical= paste0('plot.data <- cbind(import.data, cdt(cat.data))'),
          mixed= paste0('plot.data <- cbind(import.data, cat.data)')), 
        if (any(NArows)) '[!NArows, ]', '\n')

      values$codetxt$traindat <- paste0(
        if (! is.null(codeTxt$NumToFact)) 
          paste0("### Transform selected numeric to factors\n", codeTxt$NumToFact),
        if (! is.null(codeTxt$FactToNum)) 
          paste0("### Coerce selected factors to numeric\n", codeTxt$FactToNum),
        codeTxt$sel, 
        if (! is.null(codeTxt$NArows)) {
          paste0("### Warning: ", err.msg$NArows, "\n", codeTxt$NArows)},
        if (! is.null(codeTxt$constant)) {
          paste0("### Warning: ", err.msg$constant, "\n", codeTxt$constant)},
        codeTxt$scale, codeTxt$join)
      
      list(dat= dat, msg= err.msg)
    })
  })


  ## Train SOM when button is hit (triggered by change in ok.traindat)
  ok.som <- reactive({
    dat <- ok.traindat()
    if (is.null(dat)) return(NULL)
    if (is.null(dat$dat)) return(NULL)
    dat <- dat$dat
    
    isolate({
      ## Repro code
      values$codetxt$train <- 
        paste0("\n## Train SOM\n", 
               "### RNG Seed (for reproducibility)\n", 
               "set.seed(", input$trainSeed, ")\n",
               "### Initialization\n", 
               'init <- somInit(train.data, ncols = ', input$kohDimx, 
               ', nrows = ', input$kohDimy, 
               if (input$kohInit != "pca.sample") {
                 paste0(', method= "', input$kohInit, '"')
               }, 
               ")\n",
               "### Training\n", 
               "the.som <- kohonen::som(train.data, grid = kohonen::somgrid(", 
               input$kohDimx, ", ", input$kohDimy, ', "', 
               input$kohTopo, '"), maxNA.fraction = ', input$trainMaxNA, 
               ', rlen = ', input$trainRlen, 
               ", alpha = c(", input$trainAlpha1, ", ", 
               input$trainAlpha2, "), radius = c(", 
               input$trainRadius1, ",", input$trainRadius2, 
               '), init = init, dist.fcts = "sumofsquares")\n')
      
      ## Initialization
      set.seed(input$trainSeed)
      init <- aweSOM::somInit(dat, input$kohDimy, input$kohDimx, input$kohInit)
      
      ## Train SOM
      res <- try(kohonen::som(
        dat, init= init, dist.fcts= "sumofsquares",
        grid= kohonen::somgrid(input$kohDimx, input$kohDimy, input$kohTopo), 
        maxNA.fraction= input$trainMaxNA,
        rlen= input$trainRlen, alpha= c(input$trainAlpha1, input$trainAlpha2), 
        radius= c(input$trainRadius1, input$trainRadius2)))
      if(class(res) == "try-error") {
        updateNumericInput(session, "trainSeed", value= sample(1e5, 1))
        return(NULL)
      }
      
      ## Save seed
      res$seed <- input$trainSeed
    })
    ## After training, set new seed value in training panel
    updateNumericInput(session, "trainSeed", value= sample(1e5, 1))
    res
  })
  
  ## Get observations clustering when ok.som changes
  ok.clust <- reactive({
    factor(ok.som()$unit.classif, 1:nrow(ok.som()$grid$pts))
  })
  
  ## Compute superclasses when ok.som or superclass options changes
  ok.hclust <- reactive({
    if(!is.null(ok.som())){
      hclust(dist(ok.som()$codes[[1]]), input$sup_clust_hcmethod)
    }
  })
  
  ok.pam_clust <- reactive({
    if(!is.null(ok.som())){
      cluster::pam(ok.som()$codes[[1]], input$kohSuperclass)
    }
  })
  
  
  ## Assign superclasses to cells
  ok.sc <- eventReactive(c(ok.som(), input$kohSuperclass, 
                           input$sup_clust_method, input$sup_clust_hcmethod), {
    if(is.null(ok.som())) return(NULL)

    if (input$sup_clust_method == "hierarchical") {
      superclasses <- unname(cutree(ok.hclust(), input$kohSuperclass))
      
      values$codetxt$sc <- paste0("## Group cells into superclasses (hierarchical clustering)\n", 
                                  'superclust <- hclust(dist(the.som$codes[[1]]), "', 
                                  input$sup_clust_hcmethod, '")\n',
                                  "superclasses <- cutree(superclust, ", 
                                  input$kohSuperclass, ")\n")
    } else {
      superclasses <- unname(ok.pam_clust()$clustering)
      
      values$codetxt$sc <- paste0("## Group cells into superclasses (PAM clustering)\n", 
                                  "superclust <- cluster::pam(the.som$codes[[1]], ", 
                                  input$kohSuperclass, ")\n",
                                  "superclasses <- superclust$clustering\n")
    }
    
    values$codetxt$sc <- paste0(values$codetxt$sc,
                                "## Apply clusterings to observations\n",
                                "obs.class <- the.som$unit.classif\n",
                                "obs.superclass <- superclasses[obs.class]\n")
    
    superclasses
  })
  
  
  ## Current training vars
  ok.trainvars <- eventReactive(ok.traindat(), {
    colnames(ok.traindat()$dat)
  })
  
  
  ## Current training rows (not too many NA)
  ok.trainrows <- eventReactive(ok.trainvars(), {
    res <- ok.data()
    for (ivar in values$varFactToNum) 
      res[, ivar] <- suppressWarnings(as.numeric(as.character(res[, ivar])))
    for (ivar in values$varNumToFact) 
      res[, ivar] <- as.factor(res[, ivar])
    qualVar <- sapply(input$trainVarChoice, function(x) !is.numeric(res[, x]))
    if (any(qualVar)) {
      traindat <- cbind(res, aweSOM::cdt(res[input$trainVarChoice[qualVar]]))[, ok.trainvars()]
    } else traindat <- res[ok.trainvars()]
    rowSums(is.na(traindat)) <= input$trainMaxNA * ncol(traindat)
  })
  
  
  ## Training message
  output$Message <- renderPrint({
    if (is.null(isolate(ok.data()))) return(cat("Import data to train a SOM."))
    if (!is.null(ok.traindat()$msg)) {
      cat(paste0("********** Warning: **********\n", 
                 paste("* ", ok.traindat()$msg, collapse= "\n"), 
                 "\n******************************\n\n"))
    }
    if (is.null(ok.traindat())) {
      return(cat("No map trained yet, click Train button."))
    } else if (is.null(ok.som())) 
      return(cat("Something went wrong during som training. "))
    
    cat("## SOM summary:\n")
    isolate(cat(paste0(
      "SOM of ", input$kohDimy, " rows and ", input$kohDimx, " columns, with a ", 
      ok.som()$grid$topo, " topology and a bubble neighbourhood function.\n",
      "Trained on ", nrow(ok.traindat()$dat) , " observations, ", 
      ncol(ok.traindat()$dat) , " variables ; ", 
      "maxNA.fraction = ", input$trainMaxNA, ".\n",
      "Training options: rlen = ", input$trainRlen, 
      " ; alpha = (", input$trainAlpha1, ", ", input$trainAlpha2, ") ; ",
      "radius = (", input$trainRadius1, ", ", input$trainRadius2, ").\n",
      "Random seed = ", ok.som()$seed, ".\n")))

    aweSOM::somQuality(ok.som(), ok.traindat()$dat)
  })
  
 
  
  #############################################################################
  ## Panel "Plot"
  #############################################################################

  ## Update plot type choices on plot "what" selection
  observe({
    input$graphWhat
    isolate({
      if (is.null(ok.sc())) return(NULL)
      updateSelectInput(session, "graphType", choices= plotChoices[[input$graphWhat]])
    })
  })
  
  ## Update max nb superclasses
  observe({
    som <- ok.som()
    updateNumericInput(session, "kohSuperclass", max= som$grid$xdim * som$grid$ydim)
  })
  
  ## ok.plotdata : contains ok.data and the dummy-encoded training qual variables
  ok.plotdata <- eventReactive(ok.trainvars(), {
    res <- ok.data()
    for (ivar in values$varFactToNum) 
      res[, ivar] <- suppressWarnings(as.numeric(as.character(res[, ivar])))
    for (ivar in values$varNumToFact) 
      res[, ivar] <- as.factor(res[, ivar])
    qualVar <- sapply(input$trainVarChoice, function(x) !is.numeric(res[, x]))
    if (!any(qualVar)) return(res)
    cbind(res, aweSOM::cdt(res[input$trainVarChoice[qualVar]]))
  })

  ## Update variable selection for graphs, if necessary
  observeEvent(ok.som(), {
    changeVars <- TRUE
    tmp.numeric <- sapply(ok.plotdata(), is.numeric)
    if (! is.null(values$previous.trainvars)) {
      if (all(c(input$plotVarMult, input$plotVarOne) %in% colnames(ok.plotdata())) &
          all(colnames(ok.plotdata())[tmp.numeric] %in% values$previous.plotvarchoices))
          changeVars <- FALSE
    }
    if (changeVars) {
      updateSelectInput(session, "plotVarOne", choices= colnames(ok.data()), 
                        selected= ifelse(any(!tmp.numeric), 
                                         colnames(ok.plotdata())[!tmp.numeric][1], 
                                         input$trainVarChoice[1]))
      updateSelectInput(session, "plotVarMult", 
                        choices= colnames(ok.plotdata())[tmp.numeric],
                        selected= ok.trainvars())
      updateSelectInput(session, "plotVarColor", 
                        choices= c("None", colnames(ok.data())), 
                        selected= "None")
      updateSelectInput(session, "plotVarTooltip", 
                        choices= colnames(ok.data()), 
                        selected= input$trainVarChoice)
      values$previous.trainvars <- ok.trainvars()
      values$previous.plotvarchoices <- colnames(ok.plotdata())[tmp.numeric]
    }
  }) 
  
  ## Select training vars if button is hit
  observeEvent(input$plotSelectTrain, {
    updateSelectInput(session, "plotVarMult", 
                      selected = ok.trainvars())
  })

  ## Rearrange variables order if "Arrange" button is hit
  observeEvent(input$plotArrange, {
    updateSelectInput(session, "plotVarMult", 
                      selected = aweSOM::aweSOMreorder(ok.som(), 
                                                       ok.plotdata()[ok.trainrows(), ], 
                                                       input$plotVarMult, 
                                                       input$contrast, 
                                                       input$average_format))
  })
  
  
  ## Populate observation names selector
  output$plotNames <- renderUI({
    if (is.null(ok.data())) return(NULL)
    isolate({
      ## Try to find rownames variable on import data change
      allunique <- sapply(ok.data(), function(x) !any(duplicated(x)))
      tmp.numeric <- sapply(ok.data(), is.numeric)
      if (any(allunique & !tmp.numeric)) {
        selected <- colnames(ok.data())[which(allunique & !tmp.numeric)[1]]
      } else {
        selected <- "(rownames)"
      }
      fluidRow(column(4, p("Observations names:")), 
               column(8, selectInput("plotNames", NULL,
                                     choices= c("(rownames)", colnames(ok.data())),
                                     selected= selected)))
    })
  })
  
  ## Update legend font size on size change
  observeEvent(input$plotSize, {
    updateNumericInput(session, "legendFontsize", 
                       value = round(14 * input$plotSize / 400))
  })
  
  ## For "Cloud" plot, disable kPCA method if nobs > 1000
  observeEvent(ok.trainrows(), {
    if (is.null(values$cloudKshown)) values$cloudKshown <- TRUE
    nowshown <- sum(ok.trainrows()) <= 1e3
    if (nowshown != values$cloudKshown) {
      values$cloudKshown <- nowshown
      if (nowshown) {
        updateSelectInput(session, "plotCloudType", 
                          choices = c("Cell-wise PCA" = "cellPCA",
                                      "Cell-centered kPCA" = "kPCA", 
                                      "Cell-centered PCA" = "PCA",
                                      "Prototype Proximity" = "proximity", 
                                      "Random" = "random"))
      } else {
        updateSelectInput(session, "plotCloudType", 
                          choices = c("Cell-wise PCA" = "cellPCA",
                                      "Cell-centered PCA" = "PCA",
                                      "Prototype Proximity" = "proximity", 
                                      "Random" = "random"))
      }
    }
  })
  
  ## Dendrogram plot
  output$plotDendrogram <- renderPlot({
    if (input$sup_clust_method != "hierarchical") 
      return(ggplot2::ggplot(data.frame(
        x= 0, y= 0, 
        label= "Dendrogram only exists for hierarchical clustering."), 
        ggplot2::aes(x, y, label= label)) + 
          ggplot2::geom_text(size = 6) + ggplot2::theme_void())
    
    values$codetxt$plot <- paste0("\n## Plot superclasses dendrogram\n", 
                                  "aweSOMdendrogram(superclust, ", 
                                  input$kohSuperclass, ")\n")
    aweSOM::aweSOMdendrogram(ok.hclust(), input$kohSuperclass)
    }, width = reactive({input$plotSize / 4 + 500}),
    height = reactive({input$plotSize / 4 + 500}))
  
  
  ## Scree plot
  output$plotScreeplot <-  renderPlot({
    values$codetxt$plot <- paste0("\n## Plot superclasses scree plot\n", 
                                  'aweSOMscreeplot(the.som, method = "', 
                                  input$sup_clust_method, '", ', 
                                  if (input$sup_clust_method == "hierarchical") {
                                    paste0('hmethod = "', input$sup_clust_hcmethod, '", ')
                                  },
                                  "nclass = ", input$kohSuperclass, ")\n")
    aweSOM::aweSOMscreeplot(ok.som(), input$kohSuperclass, input$sup_clust_method, input$sup_clust_hcmethod)
  },
  width = reactive({input$plotSize / 4 + 500}),
  height = reactive({input$plotSize / 4 + 500}))
  

  ## Silhouette plot
  output$plotSilhouette <- renderPlot({
    values$codetxt$plot <- paste0("\n## Plot superclasses silhouette plot\n", 
                                  "aweSOMsilhouette(the.som, superclasses)\n")
    aweSOM::aweSOMsilhouette(ok.som(), ok.sc())
  },
  width = reactive({input$plotSize / 4 + 500}),
  height = reactive({input$plotSize / 4 + 500}))
  
  
  ## Smooth distance plot
  output$plotSmoothDist <-  renderPlot({
    if (ok.som()$grid$xdim < 2 || ok.som()$grid$ydim < 2 ) {
      return(ggplot2::ggplot(data.frame(
        x= 0, y= 0, 
        label= "Smooth distance plot can only \nbe drawn for two-dimensional maps, \n ie with at least 2 rows and 2 columns."), 
        ggplot2::aes(x, y, label= label)) + 
          ggplot2::geom_text(size = 6) + ggplot2::theme_void())
    }
    values$codetxt$plot <- paste0("\n## Plot smooth neighbour distances\n", 
                                  "aweSOMsmoothdist(the.som",
                                  if (input$palplot != "viridis") {
                                    paste0(', pal = "', input$palplot, '"')
                                  },
                                  if (input$plotRevPal) {
                                    ", reversePal = TRUE"
                                  },
                                  ")\n")
    aweSOM::aweSOMsmoothdist(som = ok.som(), pal = input$palplot, 
                             reversePal = input$plotRevPal, 
                             legendFontsize = input$legendFontsize)
  },
  width = reactive(min(1, ok.som()$grid$xdim / ok.som()$grid$ydim) *
                     input$plotSize + 13 * input$legendFontsize),
  height = reactive(min(1, ok.som()$grid$ydim / ok.som()$grid$xdim) *
                      input$plotSize))
  
  
  ## Fancy JS plots through widget
  output$theWidget <- aweSOM:::renderaweSOM({
    if (is.null(input$plotNames)) return(NULL) # Prevents error due to not-yet loaded UI element, for reproducible script
    if (is.null(ok.som())) return(NULL)
    if (input$graphType %in% c("Circular", "Barplot", "Line", "Radar"))
      if (input$average_format == "prototypes") if(!any(input$plotVarMult %in% ok.trainvars())) {
        warning("Prototypes plot: none of the selected variables are training variables.")
        return(NULL)
      }
    
    ## Reproducible script for plot
    values$codetxt$plot <- paste0(
      "\n## Interactive plot\n", 
      'aweSOMplot(som = the.som, type = "', input$graphType, '", ', 
      if (! (input$graphType %in% c("Hitmap", "UMatrix"))) {
        "data = plot.data, "
      }, 
      "\n",
      if (input$graphType %in% c("Circular", "Barplot", "Boxplot", "Line", "Radar")) {
        paste0('           variables = c("', paste(input$plotVarMult, collapse= '", "'), '"),\n')
      },
      if (input$graphType %in% c("Color", "Pie", "CatBarplot")) {
        paste0('           variables = "', input$plotVarOne, '",\n')
      },
      if (input$graphType == "Cloud") {
        paste0('           variables = c("', 
               paste0(c(input$plotVarColor, input$plotVarTooltip), collapse = '", "'),
               '"),\n')
      },
      "           superclass = superclasses, ", 
      if (input$plotNames != "(rownames)") {
        paste0('obsNames = plot.data[, "', input$plotNames, '"], ')
      }, 
      "\n",
      if (input$graphType %in% c("Circular", "Line", "Barplot", "Boxplot", "Color", "UMatrix", "Radar") && input$contrast != "contrast") {
        paste0('           scales = "', input$contrast, '",\n')
      },
      if (input$graphType %in% c("Circular", "Line", "Barplot", "Boxplot", "Color", "UMatrix", "Radar") && input$average_format != "mean") {
        paste0('           values = "', input$average_format, '",\n')
      },
      if (input$palsc != "Set3") {
        paste0('           palsc = "', input$palsc, '", \n')
      },
      if (input$palplot != "viridis") {
        paste0('           palvar = "', input$palplot, '", \n')
      }, 
      if (input$plotRevPal) {
        paste0("           palrev = ", input$plotRevPal, ", \n")
      },
      if (input$graphType == "Boxplot" && !input$plotOutliers) {
        "           boxOutliers = FALSE,\n"
      },
      if (input$graphType %in% c("Color", "UMatrix") && !input$plotShowSC) {
        "           showSC = FALSE,\n"
      },
      if (input$graphType %in% c("Hitmap", "Circular", "Barplot", "Boxplot", "CatBarplot", "Radar") && !input$plotTransparency) {
        "           transparency = FALSE,\n"
      },
      if (input$graphType %in% c("Circular", "Line", "Barplot", "Boxplot", "CatBarplot", "Radar") && !input$plotAxes) {
        "           showAxes = FALSE,\n"
      },
      if (input$graphType == "Pie" && input$plotEqualSize) {
        paste0("           plotEqualSize = TRUE,\n") 
      },
      if (!input$plotShowNames) {
        paste0("           showNames = FALSE,\n") 
      },
      if (input$graphType %in% c("Cloud", "Circular", "Line", "Barplot", "Boxplot", "CatBarplot", "Pie", "Color", "UMatrix") && input$legendPos != "beside") {
        paste0('           legendPos = "' , input$legendPos, '",\n')
      },
      if (input$graphType %in% c("Cloud", "Circular", "Line", "Barplot", "Boxplot", "CatBarplot", "Pie", "Color", "UMatrix") && input$legendFontsize != 14) {
        paste0('           legendFontsize = ' , input$legendFontsize, ',\n')
      },
      if (input$graphType == "Cloud" && input$plotCloudType != "cellPCA") {
        paste0('           cloudType = "' , input$plotCloudType, '",\n')
      },
      if (input$graphType == "Cloud" && input$plotCloudType == "random") {
        paste0('           cloudSeed = ' , input$plotCloudSeed, ',\n')
      },
      "           size = ", input$plotSize, ")")
    
    aweSOM:::aweSOMwidget(ok.som= ok.som(), 
                          ok.sc= ok.sc(), 
                          ok.data= ok.plotdata(), 
                          ok.trainrows= ok.trainrows(), 
                          graphType= input$graphType, 
                          plotNames= input$plotNames, 
                          plotVarMult= if (input$graphType == "Cloud") {
                            if (input$plotShowTooltip) {
                              input$plotVarTooltip
                            } else NULL
                          } else {
                            input$plotVarMult
                          },
                          plotVarOne= ifelse(input$graphType == "Cloud", 
                                             input$plotVarColor, 
                                             input$plotVarOne), 
                          plotSize= input$plotSize, 
                          plotOutliers= input$plotOutliers,
                          plotEqualSize= input$plotEqualSize,
                          plotShowSC= input$plotShowSC, 
                          contrast= input$contrast, 
                          average_format= input$average_format,
                          palsc= input$palsc, 
                          palplot= input$palplot, 
                          plotRevPal= input$plotRevPal, 
                          plotAxes= input$plotAxes,
                          plotTransparency= input$plotTransparency, 
                          legendPos= input$legendPos,
                          legendFontsize= input$legendFontsize, 
                          cloudType= input$plotCloudType, 
                          cloudSeed= input$plotCloudSeed)
  })
  
  ## Download interactive plot (download widget)
  output$downloadInteractive <- downloadHandler(
    filename= paste0(Sys.Date(), "-aweSOM.html"), 
    content= function(file) {
      if (is.null(ok.som())) return(NULL)
      widg <- aweSOM::aweSOMplot(som= ok.som(), type = input$graphType,
                                 data = ok.plotdata()[ok.trainrows(), , drop = FALSE], 
                                 variables = if (input$graphType %in% c("Color", "Pie", "CatBarplot")) {
                                   input$plotVarOne
                                 } else if (input$graphType == "Cloud") {
                                   if (input$plotShowTooltip) {
                                     c(input$plotVarColor, input$plotVarTooltip)
                                   } else input$plotVarColor
                                 } else {
                                   input$plotVarMult
                                 },
                                 superclass = ok.sc(), 
                                 obsNames = if (input$plotNames != "(rownames)") {
                                   ok.data()[, input$plotNames]
                                 } else {
                                   NULL
                                 }, 
                                 scales = input$contrast, 
                                 values = input$average_format, 
                                 size = input$plotSize, palsc = input$palsc, 
                                 palvar = input$palplot, palrev = input$plotRevPal, 
                                 showAxes = input$plotAxes, 
                                 transparency = input$plotTransparency, 
                                 boxOutliers = input$plotOutliers,
                                 showSC = input$plotShowSC, 
                                 pieEqualSize = input$plotEqualSize, 
                                 showNames = input$plotShowNames, 
                                 legendPos = input$legendPos, 
                                 legendFontsize = input$legendFontsize, 
                                 cloudType = input$plotCloudType, 
                                 cloudSeed = input$plotCloudSeed)
      
      htmlwidgets::saveWidget(widg, file = file)
    }) 
  

  
  #############################################################################
  ## Panel "Clustered Data"
  #############################################################################
  
  ## Update choices for rownames column
  output$clustVariables <- renderUI({
    if (is.null(ok.sc())) return()
    isolate(selectInput(inputId= "clustVariables", label= NULL, multiple= TRUE,
                        choices= c("Superclass", "SOM.cell", colnames(ok.data())),
                        selected= c("Superclass", "SOM.cell", colnames(ok.data())[1])))
  })
  
  ## Update choices for rownames column on button clicks
  observeEvent(input$clustSelectNone, {
    if (is.null(ok.sc())) return()
    updateSelectInput(session, "clustVariables", selected= c("Superclass", "SOM.cell"))
  })
  observeEvent(input$clustSelectAll, {
    if (is.null(ok.sc())) return()
    updateSelectInput(session, "clustVariables", 
                      selected= c("Superclass", "SOM.cell", isolate(colnames(ok.data()))))
  })
  observeEvent(input$clustSelectTrain, {
    if (is.null(ok.sc())) return()
    varSelected <- colnames(ok.data()) %in% input$trainVarChoice
    updateSelectInput(session, "clustVariables", 
                      selected= c("Superclass", "SOM.cell", 
                                  colnames(ok.data())[varSelected]))
  })
  

  ## Current clustered data table
  ok.clustTable <- eventReactive(c(ok.sc(), input$clustVariables), {
    if (is.null(ok.sc()) | is.null(input$clustVariables)) return()
    res <- data.frame(ok.data(), SOM.cell= NA, Superclass= NA)
    res[ok.trainrows(), "SOM.cell"] <- ok.clust()
    res[ok.trainrows(), "Superclass"] <- ok.sc()[ok.clust()]
    res[, input$clustVariables]
  })

  ## Display clustered data  
  output$clustTable <- DT::renderDataTable(ok.clustTable())

  ## Download clustered data
  output$clustDownload <- 
    downloadHandler(filename= paste0("aweSOM-clust-", Sys.Date(), ".csv"), 
                    content= function(con) write.csv(ok.clustTable(), con)) 
  
  ## Download som object (rds)
  output$somDownload <- 
    downloadHandler(filename= paste0("aweSOM-som-", Sys.Date(), ".rds"), 
                    content= function(con) saveRDS(ok.som(), con))
  
  
  #############################################################################
  ## Panel "Reproducible code"
  #############################################################################
  
  reprocode <- reactive({
    paste0("library(aweSOM) # (version 1.3) \n\n",
           values$codetxt$dataread, 
           values$codetxt$traindat, 
           values$codetxt$train,
           if (!is.null(ok.som())) paste0(
             "\n## Quality measures\n",
             "somQuality(the.som, train.data)\n\n",
             values$codetxt$sc, 
             values$codetxt$plot))
  })
  
  output$codeTxt <- renderText(reprocode())
  
  output$copycode <- renderUI({
    rclipboard::rclipButton("copycodebtn", "Copy to clipboard", reprocode())
  })
  
  
  output$report <- downloadHandler(
    filename = "aweSOM-report.html",
    content = function(file) {
      if (is.null(ok.som())) return(NULL)
      tempReport <- file.path(tempdir(), "reproducible_code.Rmd")
      file.copy("reproducible_code.Rmd", tempReport, overwrite = TRUE)
      rmarkdown::render(tempReport, output_file = file,
                        params = list(code = values, ok.data = ok.data()),
                        envir = new.env(parent = globalenv())
      )
    }
  )

})

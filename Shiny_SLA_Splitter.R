#https://www.r-bloggers.com/2019/07/excel-report-generation-with-shiny/
library("ERICDataProc")


options(shiny.maxRequestSize=500*1024^2)

  # minimal Shiny UI
ui <- fluidPage(
  titlePanel("SLA splitter"),
  tags$br(),

  fileInput("slafile", "Choose SLA File",
            accept = c(
              "application/vnd.ms-excel",
              "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
              ".xlsx")
  ),

  radioButtons(inputId = "la",label="Local authority",c("Durham"=1, "Gateshead"=2,"Northumberland"=3,"South Tyneside"=4,"Sunderland"=5)),

  textInput(inputId = "outputfile",label = "Output filename",value = ""),
  textOutput(outputId = "msg"),



  downloadButton(
    outputId = "okBtn",
    label = "Split SLA data")


)

# minimal Shiny server
server <- function(input, output) {


  output$okBtn <- downloadHandler(
    filename = function() {
      ifelse(str_ends(input$outputfile,'xlsx'),input$outputfile,paste0(input$outputfile,'.xlsx'))

    },
    content = function(file) {

      #SLA_config <- getSLAConfig(as.numeric(input$la))
      config <- setup_splitter_config_values ()

      SLA_config <- getSplitterConfig(as.numeric(input$la),config)

      sourceSheet <- SLA_config[[1]]
      allDataSheet <- SLA_config[[2]]
      SLA_split <- SLA_config[[3]]
      outputCols <- SLA_config[[4]]

      #Create output workbook
      XL_wb <- createWorkbook()


      inFile <- input$slafile

      if (is.null(inFile))
        return(NULL)

      #Assume only a single sheet
      SLA_data <- read_excel(inFile$datapath,sheet = 1, col_names = TRUE,col_types = "text")

      #Format the date
      SLA_data$Date <- formatDates(SLA_data$Date)
      SLA_data$'Obs Changed Date' <- formatDates(SLA_data$'Obs Changed Date')
      SLA_data$`Obs Entry Date` <- formatDates(SLA_data$`Obs Entry Date`)

      #Output the data
      XL_wb <- createWorkbook()
      addWorksheet(XL_wb,sourceSheet)
      writeData(XL_wb,sourceSheet,SLA_data)

      #Remove vague grid refs
      SLA_data<- filter(SLA_data,as.numeric(SLA_data$Precision)<10000)

      #All data gets added to the All data sheet too
      if (allDataSheet != "") {
        addWorksheet(XL_wb,allDataSheet)
      }

      split_data_to_sheets(SLA_split,XL_wb,SLA_data,allDataSheet,outputCols)


      # Write the workbook to the file
      #Put the all data sheets at the end
      worksheetOrder(XL_wb) <- c(worksheetOrder(XL_wb)[-2],2)
      saveWorkbook(XL_wb,file,overwrite = TRUE)
    }
  )


}

shinyApp(ui, server)

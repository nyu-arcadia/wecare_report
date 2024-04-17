library(shiny)
library(shinydashboard)
library(officer)
library(officedown)

ui <- dashboardPage(
  dashboardHeader(title = "WeCare Report"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    uiOutput("loginPage"),
    uiOutput("mainUI")
  )
)

server <- function(input, output, session) {
  USER <- reactiveValues(loggedIn = FALSE)
  
  observe({
    if (isTruthy(input$btnLogin)) {
      if (input$pwd == "wecare") {
        USER$loggedIn <- TRUE
      } else {
        showNotification("Wrong password!", type = "error")
      }
    }
  })
  
  
  output$loginPage <- renderUI({
    if (!USER$loggedIn) {
      tagList(
        passwordInput("pwd", "Enter password"),
        actionButton("btnLogin", "Login")
      )
    }
  })
  
  output$mainUI <- renderUI({
    if (USER$loggedIn) {
      tagList(
        actionButton("generateReport", "Generate Report"),
        downloadButton("downloadReport", "Download Report")
      )
    }
  })
  
  reportPath <- reactiveVal()
  observeEvent(input$generateReport, {
    withProgress({
      setProgress(message = "Generating report...", value = 0.5)
      # Assume this function takes some time and finally generates the report
      reportPath(rmarkdown::render("inst/progress_report.Rmd", output_format = "officedown::rdocx_document"))
      setProgress(value = 1)
      # Show notification that the report has been successfully generated
      showNotification("Report has been generated, please download it to your computer.", type = "message")
    })
  })
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste("report-", Sys.Date(), ".docx", sep = "")
    },
    content = function(file) {
      file.copy(reportPath(), file)
    }
  )
}

shinyApp(ui = ui, server = server)
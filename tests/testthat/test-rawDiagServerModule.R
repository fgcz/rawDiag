#R

#' @noRd
#' @references  https://shiny.posit.co/r/articles/improve/server-function-testing/
shiny::testServer(rawDiagServer,
                  args = list(vals = list(rawfile =  rawrr::sampleFilePath())),
{
  
  expect_true(file.exists(rawfile()))
  expect_true(is.data.frame(data()))
  
  
  session$setInputs(plotFun = "plotInjectionTime")
  session$setInputs(plotArg = "trellis")
  session$setInputs(plotHeight = 400)
  
  expect_true("MSOrder" %in% colnames(data()))
  expect_true("StartTime" %in% colnames(data()))
  expect_true("BasePeakIntensity" %in% colnames(data()))
  expect_true("rawfile" %in% colnames(data()))
  
  expect_true(input$plotFun %in% ls("package:rawDiag")) 
  expect_equal(nrow(data()), 574)
  expect_equal(ncol(data()), 18)
})

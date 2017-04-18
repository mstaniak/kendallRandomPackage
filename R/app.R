#' Shiny app for exploring air pollution data
#'
#' @param sourceFrame tibble returned by calculateMaxima function.
#'
#' @return shiny app object
#'
#' @export 
#' 
#' @examples
#' obs <- importGiosFromXLSX("DsWrocKorzA", c("NOx", "SO2"), c("2015", "2014"))
#' obs2 <- calculateMaxima(obs)
#' kendallRandomApp(obs2)
#'

kendallRandomApp <- function(sourceFrame) {
  require(shiny)
  allYears <- unique(sourceFrame$year)
  names(allYears) <- allYears
  allPolutants <- unique(sourceFrame$polutant)
  names(allPolutants) <- unique(sourceFrame$polutant)

  maxThreshold <- sourceFrame %>%
    dplyr::filter(polutant == allPolutants[1],
	         year == allYears[1]) %>%
    dplyr::ungroup() %>%
    dplyr::select(maximum) %>%
    unlist(use.names = FALSE) %>%
    max(na.rm = T)
  
  shinyApp(
    ui =  fluidPage(
    navbarPage("Modelling air pollution levels in Poland",
	       id = "wholePage",
#       tabPanel("About",
# 	fluidRow(column(12, id = "aboutApp"))),
	       # wyświetlić nazwę stacji.
      tabPanel("Overview",
	       value = "overview",
	fluidRow(column(2, id = "overviewInputs",
			selectInput("plotTypeO", label = "Type of a plot",
				    choices = c("Obs. over time" = "time",
						"Histogram" = "hist",
						"Empirical CDF" = "ecdf")),
			selectInput("chosenPolutantO", label = "Polutant",
				    choices = allPolutants, selected = allPolutants[1]),
			selectInput("chosenYearO", label = "Year",
				    choices = allYears, selected = max(allYears))
		 ),
		 column(10, id = "overviewOutput",
			plotOutput("overviewPlot")
		 ))),
      tabPanel("Peak-over-threshold",
	       value = "pot",
	fluidRow(column(2, id = "potInputs",
                 selectInput("plotTypePOT", label = "Type of a plot",
			     choices = c("Histogram" = "hist",
					 "QQ-plot" = "qqplot",
					 "Empirical CDF" = "ecdf")),
		 selectInput("chosenPolutant", label = "Polutant",
			     choices = allPolutants, selected = allPolutants[1]),
		 sliderInput("threshold", label = "threshold",
			     min = 0, max = maxThreshold, value = maxThreshold/10),
		 sliderInput("alphaPOT", label = "Alpha parameter",
			     min = 0.01, max = 0.99, value = 0.5), 
		 selectInput("chosenYear", label = "Year",
			     choices = allYears, selected = max(allYears))
		 ),
		 column(10, id = "potOutput",
			plotOutput("histPOT")
		 ))),
      tabPanel("Tails",
	       value = "largeqq",
	fluidRow(column(2, id = "tailsInputs",
		 # Poznać metody dopasowania kwantyli dużego rzędu i zaimplementować tu.
		 selectInput("chosenPolutantT", label = "Polutant",
			     choices = allPolutants, selected = allPolutants[1]),
		 selectInput("chosenYearT", label = "Year",
			     choices = allYears, selected = max(allYears)),
		 sliderInput("alpha", label = "Alpha parameter",
			     min = 0, max = 1, value = 0.5),
		 sliderInput("quantiles", label = "Minimum and maximum quantiles",
			     min = 0.6, max = 1, value = c(0.8, 0.999)),
		 sliderInput("quantileStep", 
			     label = "Step between min and max qunatiles",
			     min = 0.01, max = 0.05, value = 0.02)
		 # Może dodać min-max kwantyl do wyboru.
		 ),
		 column(10, id = "tailsOutput",
			plotOutput("largeQuantiles")
		 ))),
      tabPanel("Distribution fitting",
	       value = "fitting",
	       fluidRow(column(2, id = "fitInputs",
			selectInput("chosenPolutantF", label = "Polutant",
				    choices = allPolutants, selected = allPolutants[1]),
			selectInput("chosenYearF", label = "Year",
				    choices = allYears, selected = max(allYears)),
			selectInput("chosenPlotF", label = "Plot",
				    choices = c("QQ-plot" = "qqplot", "CDF vs ECDF" = "cdfs"))

		 ),
		 column(10, id = "fitOutput",
			 plotOutput("fitted"),		   
			 tableOutput("fittedParameters")
		 ))), 
      tabPanel("Kendall random walk",
	fluidRow(column(2, id = "kendallInputs",
			sliderInput("trajectoriesNumber", label = "Number of trajectories",
				    min = 1, max = 1000, value = 10),
			sliderInput("trajectoriesLength", label = "Length of a trajectory",
				    min = 1, max = 100000, value = 1000),
			sliderInput("parAlphaK", label = "Alpha parameter",
				    min = 0, max = 1, value = 0.5)
			),
		 column(10, id = "kendallOutputs",
			plotOutput("kendallPlot")
			)))
)),
	   server = function(input, output, session) {
	     filteredData <- reactive({
	if(input$wholePage == "largeqq") chosenPolutant <- input$chosenPolutantT
	else if(input$wholePage == "pot") chosenPolutant <- input$chosenPolutant
	else if(input$wholePage == "overview") chosenPolutant <- input$chosenPolutantO
	else if(input$wholePage == "fitting") chosenPolutant <- input$chosenPolutantF
	else chosenPolutant <- allPolutants[1]
	if(input$wholePage == "largeqq") chosenYear <- input$chosenYearT
	else if(input$wholePage == "pot") chosenYear <- input$chosenYear
	else if(input$wholePage == "overview") chosenYear <- input$chosenYearO
	else if(input$wholePage == "fitting") chosenYear <- input$chosenYearF
	else chosenYear <- allYears[1]

	sourceFrame %>%
	  dplyr::filter(polutant == chosenPolutant,
		 year == chosenYear)
      })

	fitGEV <- reactive({
		   filteredData() %>%
	     dplyr::filter(is.finite(maximum)) %>%
	     dplyr::ungroup() %>%
	     dplyr::select(maximum) %>%
		   unlist(use.names = FALSE) %>%
		   egevd()
	})

      kendallRandomWalk <- reactive({
	simulation(input$trajectoriesNumber, input$trajectoriesLength,
		   {function(n) rep(1, n)}, input$parAlphaK)
      })

      newMax <- reactive({
	max(filteredData()$maximum, na.rm = T)
      })

      observe({
	updateSliderInput(session, "threshold", max = newMax(), value = newMax()/10)
      })

      output$histPOT <- renderPlot({
	if(input$plotTypePOT == "hist") plotHist(filteredData(), input$threshold)
	else if(input$plotTypePOT == "qqplot") plotQQ(filteredData(), input$alphaPOT, input$threshold)
	else filteredData() %>%
    dplyr::filter(maximum > input$threshold) %>%
    dplyr::mutate(maximum = maximum - input$threshold) %>%
	  plotEcdf()
      })
      output$largeQuantiles <- renderPlot({
	plotLargeQQ(filteredData(), input$alpha, input$quantiles, input$quantileStep)
      })
      output$overviewPlot <- renderPlot({
	if(input$plotTypeO == "time") plotTime(filteredData())
	else if(input$plotTypeO == "hist") plotHist(filteredData(), 0)
	else plotEcdf(filteredData())
      })
      output$kendallPlot <- renderPlot({
	convergenceVis(normingSequences(kendallRandomWalk()))
      })
      output$fitted <- renderPlot({
	if(input$chosenPlotF == "qqplot") qqPlotGev(filteredData(), fitGEV())
	else cdfsGev(filteredData(), fitGEV()) 
      })
      output$fittedParameters <- renderTable(tibble(location = fitGEV()$parameters[1],
                                                        scale = fitGEV()$parameters[2],
                                                        shape = fitGEV()$parameters[3]))
    }
    )
}

#' Shiny app for exploring air pollution data
#'
#' @param sourceFrame tibble returned by calculateMaxima function.
#'
#' @return shiny app object
#'
#' @export 
#'

kendallRandomApp <- function(sourceFrame) {
  require(shiny)
  allYears <- unique(sourceFrame$year)
  names(allYears) <- allYears
  allPolutants <- unique(sourceFrame$polutant)
  names(allPolutants) <- unique(sourceFrame$polutant)

  maxThreshold <- sourceFrame %>%
    filter(polutant == allPolutants[1],
	   year == allYears[1]) %>%
  select(maximum) %>%
  unlist(use.names = FALSE) %>%
  max(na.rm = T)
  shinyApp(
    ui =  fluidPage(
    navbarPage("Modelling air pollution levels in Poland",
	       id = "wholePage",
      tabPanel("About",
	fluidRow(column(12, id = "aboutApp"))),
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
	fluidRow(column(2, id = "fitInputs"
		 ),
		 column(10, id = "fitOutput"
		 # Histogramy, dystrybuanty empiryczne i teoretyczne, wykresy qq.
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
	else chosenPolutant <- "CO"
	if(input$wholePage == "largeqq") chosenYear <- input$chosenYearT
	else if(input$wholePage == "pot") chosenYear <- input$chosenYear
	else if(input$wholePage == "overview") chosenYear <- input$chosenYearO
	else chosenYear <- "2016"

	sourceFrame %>%
	  filter(polutant == chosenPolutant,
		 year == chosenYear)
      })

      Z <- function(x,y){
	min(x,y)/max(x,y)
      }

      Qn <- function(x, y, alfa){
	p <- Z(abs(x), abs(y))^alfa
	sample(c(0,1), 1, prob=c(1-p, p))
      }

      U <- function(x, y){
	if(abs(x)>abs(y)) sign(x)
	else sign(y)
      }

      simulateOneTrajectory <- function(trajectoryLength, stepDist,
					parAlpha, ...) {
	Y <- stepDist(trajectoryLength, ...)
	theta <- rpareto( trajectoryLength, 1, 2*parAlpha)*sample(c(-1, 1), trajectoryLength, prob = c(0.5, 0.5), replace = TRUE)
	Xn <- vector("numeric", trajectoryLength )
	Xn[1:2] <- c(0, Y[1])

	for(i in 2:(trajectoryLength - 1)) {
	  Xn[i+1] <- max(abs(Xn[i]), abs(Y[i+1]))*theta[i]^Qn(Xn[i], 
							      Y[i+1], 
							      parAlpha)*U(Xn[i], Y[i + 1])
	}
	Xn
      }

      simulation <- function(simulationNumber, trajectoryLength,
			     stepDist, parAlpha, ...) {
	listTmp <- as.list(1:simulationNumber)
	tmp <- lapply(listTmp, function(l) 
		      simulateOneTrajectory(trajectoryLength, stepDist,
					    parAlpha, ...))
	lapply(listTmp, 
	       function(x) tibble(simNo = x, sim = tmp[[x]])) %>%
	bind_rows() 
      } 

      normingSequences <- function(simulations, AnSeq = 1, BnSeq = 0) {
	simulations %>%
	  mutate(simNo = as.factor(as.character(simNo))) %>%
	  group_by(simNo) %>%
	  mutate(sim = AnSeq*sim - BnSeq)
      }

      convergenceVis <- function(simulations, ogrX = NULL) {
	nSim <- max(unique(as.integer(as.character(simulations$simNo))))
	trajectoryLength <- dim(simulations)[1]/nSim
	if(is.null(ogrX)) ogrX <- trajectoryLength
	simulations %>%
	  ungroup() %>%
	  mutate(x = rep(1:trajectoryLength, nSim)) %>%
	  filter(x <= ogrX) %>% 
	  ggplot(aes(x = x, y = sim, group = simNo)) +
	  geom_line() +
	  theme_bw() +
	  xlab("") +
	  ylab("") +
	  guides(color = "none")
      }

      kendallRandomWalk <- reactive({
	simulation(input$trajectoriesNumber, input$trajectoriesLength,
		   {function(n) rep(1, n)}, input$parAlphaK)
      })

      plotHist <- function(srcTbl, threshold) {
	src <- srcTbl %>%
	  filter(maximum > threshold) %>%
	  mutate(maximum = maximum - threshold)
	binW <- IQR(src$wartosc)/(length(src$maximum)^(1/3))
	ggplot(src, aes(x = maximum)) +
	  geom_histogram(binwidth = binW) +
	  theme_bw()
      }

      plotLargeQQ <- function(srcTbl, quantiles, alpha, minMaxQ, stepQ) {
	qSeq <- seq(minMaxQ[1], minMaxQ[2], stepQ)
	x <- srcTbl %>%
	  mutate(maximum = as.vector(scale(maximum))) %>%
	  filter(is.finite(maximum)) %>%
	  select(maximum) %>%
	  unlist(use.names = FALSE) %>%
	  quantile(probs = qSeq)
	qGran <- qgraniczny(function(x) x)
	y <- qGran(qSeq, alpha)
	tibble(x = x, y = y) %>%
	  filter(is.finite(x),
		 is.finite(y),
		 x < 10,
		 y < 10) %>%
	ggplot(aes(x, y, label = round(y, 2))) +
	geom_point() +
	geom_smooth(method = "lm", se = FALSE) +
	geom_text() +
	theme_bw()
      }

      plotQQ <- function(srcTbl, alpha, threshold = 0) {
	x <- srcTbl %>%
	  filter(is.finite(maximum),
		 maximum > threshold) %>%
	mutate(maximum = maximum - threshold) %>%
	mutate(maximum = as.vector(scale(maximum))) %>%
	select(maximum) %>%
	unlist(use.names = FALSE) %>%
	quantile(probs = seq(0.1, 0.9, 0.1)) # Do poprawy
      qGran <- qgraniczny(function(x) x)
      y <- qGran(seq(0.1, 0.9, 0.1), alpha)
      tibble(x = x, y = y) %>%
	ggplot(aes(x, y)) +
	geom_point() +
	geom_smooth(method = "lm", se = FALSE) +
	theme_bw()
      }

      plotTime <- function(srcTbl, datesRange = "") {
	srcTbl %>%
	  #     filter(dzienPomiaru >= datesRange[1],
	  # 	   dzienPomiaru <= datesRange[2]) %>%
	  ggplot(aes(x = measTime, y = maximum)) +
	  geom_line() +
	  theme_bw() +
	  xlab("date") +
	  ylab("measured value")
      }

      plotEcdf <- function(srcTbl) {
	ggplot(srcTbl, aes(x = maximum)) +
	  stat_ecdf() +
	  theme_bw()
      }

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
	  filter(maximum > input$threshold) %>%
	  mutate(maximum = maximum - input$threshold) %>%
	  plotEcdf()
      })
      output$largeQuantiles <- renderPlot({
	plotLargeQQ(filteredData(), seq(0.8, 0.99, 0.05), input$alpha, input$quantiles, input$quantileStep)
      })
      output$overviewPlot <- renderPlot({
	if(input$plotTypeO == "time") plotTime(filteredData())
	else if(input$plotTypeO == "hist") plotHist(filteredData(), 0)
	else plotEcdf(filteredData())
      })
      output$kendallPlot <- renderPlot({
	convergenceVis(normingSequences(kendallRandomWalk()))
      })
    }
    )


}

#' Establish an Environment
#'
#' Distinguishing mark or seal is one of the definitions of the wor cachet.
#' As an environment within this package, cachet will provide a place to
#' assign global variables within this package's functions.
#' @examples
#' assign("evaluations",lookups, envir=cachet) # Notice the quotes.
#' assign("rngSeed",.Random.seed,envir=cachet) # Store current random seed
cachet <- new.env()
cachet$evaluations <- 0     # running total of lookups
cachet$randomNetMetMatrix <- matrix(0,nrow=17,ncol=40)
cachet$synthNetMetMatrix <- matrix(0,nrow=17,ncol=40)
## cachet$rv <- numeric()      # a list of random variates

#' Set Goals for the Metrics
#'
#' setMetricGoals receives two sets of metrics and calculates one minus
#' the ratio of the difference over the first metrics set.
#' @param metA a set of metrics for a real world network
#' @param metB a set of metrics for a random or synthesized network
setMetricGoals <- function(metA, metB) {

  metA<- sapply(metA,function(x) if(x == 0) {x = 1} else { x = x })
  portions <- 1 - (abs(metA - metB) / metA)
  portions <- sapply(portions,function(x) if(x == 1) {x = 0.999} else { x = x })

  return(portions)
}
#' Make an Myers Briggs Type Indicator
#'
#' This function constructs an Myers Briggs Type Indicator (MBTI) in
#' accordance with the following table:
#' Extroverts 46.3%	or Introverts 53.7%
#' iNtuitive  31.9%	or Sensing    68.1%
#' Thinking   52.9%	or Feeling    47.1%
#' Judging    58.1%	or Perceiving 41.9%
#' Hammer, Allen L., and Wayne D. Mitchell. "The distribution of MBTI types
#' in the US by gender and ethnic group." Journal of Psychological Type 37
#' (1996): 2-15.
makeMBTI <- function() {
  introverted <- 0.537
  sensing     <- 0.681
  thinkers    <- 0.529
  judges      <- 0.581
  mbti <- ""

   if(runif(1) >= introverted) {
      mbti <- "I"   # Introvert
   } else {
      mbti <- "E"   # Extrovert
   }
   if(runif(1) >= sensing) {
      mbti <- paste0(mbti,"S")   # Sensing
   } else {
      mbti <- paste0(mbti,"N")   # iNtuitive
   }
   if(runif(1) >= thinkers) {
      mbti <- paste0(mbti,"T")   # Thinking
   } else {
      mbti <- paste0(mbti,"F")   # Feeling
   }
   if(runif(1) >= judges) {
      mbti <- paste0(mbti,"J")   # Judging
   } else {
      mbti <- paste0(mbti,"P")   # Perceiving
   }
   return(mbti)
}
#' Calculate Metrics
#'
#' Dozens of metrics are available in the igraph package. This function
#' accepts an igraph object and returns a column of metrics for use by
#' the comparison table generator function in this package.
#' @param socialnetwork An igraph object
#' @return A column of metrics
#' @export
#' @examples
#' sn <- realnetwork("sociomatrix.txt")   # Read sociomatrix to make a network.
#' sn.metrics <- calculateMetrics(sn)     # Calculate a set of metrics.
calculateMetrics <- function(socialnetwork){
   metcalcs <- get("evaluations", envir=cachet)  # get current total

   metric.numNodes <- igraph::vcount(socialnetwork)
   metric.numLinks <- igraph::ecount(socialnetwork)
   metric.comps <- igraph::count_components(socialnetwork)
   metric.density <- igraph::edge_density(socialnetwork)
   metric.avgDeg <- mean(igraph::degree(socialnetwork))
   metric.sdDeg <- sd(igraph::degree(socialnetwork))
   metric.gcc <- igraph::transitivity(socialnetwork)
   metric.acc <- igraph::transitivity(socialnetwork, type="average")
   metric.mpl <- igraph::mean_distance(socialnetwork, unconnected=FALSE)
   metric.avgBtwn <- mean(igraph::betweenness(socialnetwork))
   metric.maxBtwn <- max(igraph::betweenness(socialnetwork))
   metric.avgClose <- mean(igraph::closeness(socialnetwork))
   metric.minClose <- min(igraph::closeness(socialnetwork))
   metric.eigcen <- igraph::eigen_centrality(socialnetwork)  # is a vector
   metric.avgEigCen <- mean(metric.eigcen$vector)
   metric.radius <- igraph::radius(socialnetwork, mode="all")
   metric.avgEccen <- mean(igraph::eccentricity(socialnetwork))
   metric.diameter <- igraph::diameter(socialnetwork)
 # Compile the results.
   metricColumn <- c(metric.numNodes,  metric.numLinks,  metric.comps,
                     metric.density,   metric.avgDeg,    metric.sdDeg,
					 metric.gcc,       metric.acc,       metric.mpl,
					 metric.avgBtwn,   metric.maxBtwn,   metric.avgClose,
					 metric.minClose,  metric.avgEigCen, metric.radius,
					 metric.avgEccen,  metric.diameter)

   metcalcs <- metcalcs + 1
   assign("evaluations",metcalcs, envir=cachet)
   return(metricColumn)
}

#' A Real Network
#'
#' A sociomatrix is a square matrix where each row and column represent
#' a person.  A one in a cell at the intersection of a row and column
#' indicates that the two people have some connection.  A zero in a cell
#' indicates that the two people do not have a connection. This function
#' reads a sociomatrix and returns a network as an igraph object.
#' @param fname Name of a text file containing a sociomatrix.
#' @return An iGraph object
#' @export
#' @examples
#' sn <- realnetwork("sociomatrix.txt")
realnetwork <- function (fname) {
 # Read a sociomatrix for a Real Social Network (RSN).
  rsn <- as.matrix(read.table(fname))
 # Ensure that it's a binary matrix to ensure single links between nodes.
  rsn <- apply(rsn, c(1,2),function(x) if (x >=1) { x = 1 } else {x = 0})
  rsn.ig <- igraph::graph_from_adjacency_matrix(rsn,mode="undirected")
  return(rsn.ig)
}

#' Average Metrics from Randomly Generated Networks
#'
#' Executing this function will randomly generate 40 networks, calculate
#' metrics, store the metrics as columns in a temporary matrix, calculate
#' the means of each row of the matrix, and return the mean metrics.
#' @param n Number of nodes
#' @param p Probability of link formation for any pair of nodes
#' @return A column of mean metrics for 40 randomly generated networks
#' @export
#' @examples
#' ravgmet <- avgRandomNetMet(40,0.32)
avgRandomNetMet <- function(n,p) {
 nMet = 17                                 # number of metrics
 iNum = 40                                 # Number of networks to generate
 metmatr <- matrix(0,nrow=nMet,ncol=iNum)  # Metrics in rows, runs in columns
 for (j in 1:iNum) {
   aRN <- igraph::sample_gnp(n,p)          # Generate a Random Network.
   rnmet <- calculateMetrics(aRN)
   metmatr[,j] <- rnmet                    # Store metrics in a column.
 }
 # Store the metrics matrix in a global variable.
 assign("randomNetMetMatrix",metmatr, envir=cachet)

 rAvgM <- rowMeans(metmatr)   # Average across the rows of the metrics matrix.
 return(rAvgM)
}
#' Mean Metrics of Synthesized Social Networks from a matrix of personalities
#'
#' Given an assignment of personalities and a compatibility table,
#' this function will synthesize 40 social networks and calculate the
#' mean metrics. This function receives a single column matrix of
#' personalities types and a personality compatibility look up table.
#' @param team A matrix that identifies a set of personality types
#' @param ct A matrix that is a personality compatibility look up table
#' @param d A desired density for the synthesized social networks
#' @return A column of mean metrics for 40 synthsized social networks
#' @export
#' @examples
#' group <- replicate(10,makeMBTI())
#' cpat <- read.csv("CustomCompatibilityTable.csv")
#' savgmet <- avgSynthNetMet(group,cpat,0.32)
avgSynthNetMet <- function(team,ctbl,netdensity) {
  nMet <- 17                              # number of metrics
  iNum <- 40                              # number of networks
  metmat <- matrix(0,nrow=nMet,ncol=iNum) # Metrics in rows, runs in columns
  indices <- match(team,colnames(ctbl))   # Compatibility table indices
  for (j in 1:iNum) {
    sm <- makeSociomatrix(indices,indices,ctbl,netdensity)
	sn <- igraph::graph_from_adjacency_matrix(sm,mode="undirected")
    snmet <- calculateMetrics(sn)
    metmat[,j] <- snmet                  # Store metrics in a column.
	rm(sm,sn,snmet)                      # Clean up for next iteration.
 } # next j
 # Store the metrics matrix in a global variable
  cachet$synthNetMetMatrix <- metmat
  snAvgMet <- rowMeans(metmat)   # Average across the rows of the metrics matrix.
  return(snAvgMet)
}

#' Mean Metrics of Synthesized Social Networks from a file of personalities
#'
#' Given an assignment of personalities and a compatibility table,
#' this function will synthesize 30 social networks and calculate the
#' mean metrics. This function reads a file that contains an assignment of
#' personalities; so, the intent of this function is to work with
#' the optimized assignment.
#' @param fn File name of a file that contains an assignment of personalities
#' @param fnct A file that contains personality compatibility table
#' @param d A desired density for the synthesized social networks
#' @return A column of mean metrics for 30 synthsized social networks
#' @export
#' @examples
#' ravgmet <- meanSynthNetMet("personalities.csv","compatibilities.csv",0.32)
meanSynthNetMet <- function(fn,fnct,d) {
  nMet <- 17                              # number of metrics
  iNum <- 40                              # number of networks
  metmat <- matrix(0,nrow=nMet,ncol=iNum)   # Metrics in rows, runs in columns
##  print(paste("Simteam file name is ",fn))
  team <- as.matrix(read.csv(fn, header=TRUE))  # Get personality assignment.
  ctbl <- as.matrix(read.csv(fnct))         # Get compatibility table.
  indices <- match(team,colnames(ctbl))     # Compatibility table indices
  ## pastSeed <- get("rngSeed",envir=cachet) # Retrieve the stored seed.
  ## set.seed(pastSeed)             # Use the seed for winning assignment.
  for (j in 1:iNum) {
    sm <- makeSociomatrix(indices,indices,ctbl,d)
	sn <- igraph::graph_from_adjacency_matrix(sm,mode="undirected")
    snmet <- calculateMetrics(sn)
    metmat[,j] <- snmet                  # Store metrics in a column.
	rm(sm,sn,snmet)                      # Clean up for next iteration.
 } # next j
 # Store the metrics matrix in a global variable
  cachet$synthNetMetMatrix <- metmat
  snAvgMet <- rowMeans(metmat)   # Average across the rows of the metrics matrix.
  return(snAvgMet)
}

#' Make a Sociomatrix
#'
#' Based upon random numbers and a compatibility table,
#' this function makes a symmetric sociomatrix.
#' @param a list of indices for the compatibility table
#' @param b list of indices for the compatibility table
#' @param lut a compatibility table
#' @return a matrix
#' @export
#' @examples
#' comptable <- as.matrix(read.csv(ctableName))
#' team <- read.csv("personalityList.csv", header=FALSE)
#' indices <- match(team,colnames(comptable)
#' sm <- makeSociomatrix(indices,indices,comptable)
makeSociomatrix <- function(a,b,lut,goaldensity) {
      n <- length(a)                 # number of nodes
      sociomatrix <- matrix(0,n,n)   # A sociomatrix is square.
	  linkbudget <- goaldensity * n*(n-1)*0.5
	  cachet$rv <- vector()         # Clear random variates

	  while(sum(sociomatrix) < 2 * linkbudget) {
         u <- sample(1:n,1) # Select a row in the sociomatrix.
         v <- sample(1:n,1) # Select a column in the sociomatrix
         i <- a[u]          # Select index for row look up.
         j <- b[v]          # Select index for column look up.

	   if (!is.na(lut[i,j])) {
	        affinity <- runif(1)
		  # Store the random variate.
		  # cachet$rv[length(cachet$rv)+1] <- round(affinity,2)
		 if(affinity > lut[i,j]) {
		    sociomatrix[u,v] <- 0
			sociomatrix[v,u] <- 0  # Make it mutual.
		 } else {
		    if(sociomatrix[u,v] == 0 & u != v) {
		       sociomatrix[u,v] <- 1
			   sociomatrix[v,u] <- 1  # Make it mutual.
			} #endif
		   }
		  } #endif
	    } #wend
	return(sociomatrix)
}

#' Monte Carlo Search for Optimum Personality Assignment
#'
#' Using real world and mean metrics of randomly generated networks,
#' this function synthesizes a population of social networks, averages
#' the metrics, and compares the mean metrics with the metrics from
#' the real world network and the randomly generated network population.
#' Results are compiled into a report and a report generator produces
#' an HTML file with a color coded table and a status report of the search.
#' @param iSampleSize Number of sociomatrices to synthesize for statistics
#' @param fnMat Input real world sociomatrix for comparison.
#' @param fnMet file name for the best metrics found during this search
#' @param lut A personality compatibility look up table
#' @param fnStat A file name for a status report from this search function
#' @param fnOpt A text file with a matrix with the optimum set of personalities
#' @param tLimit A time limit in number of seconds
#' @return This function does not return anything
#' @export
#' @examples
#' cpat <- as.matrix(read.csv("CompatibilityTable.csv")
#' rb <- realnetwork("RobinsBank.txt")  # Generate an igraph object
#' rbmet <- calculateMetrics(rb)
#' rgmet <- avgRandomNetMet(rbmet[1],rbmet[4])
#' mcSearch(rbmet,rgmet,cpat,"MC_SearchStatus.csv","OptimumSet.csv")
mcSearch <- function(iSampleSize,fnMat,fnMet,ct, fnStat,fnOpt,fnR,fnS, tLimit) {
  wins <- 0        # Number of synthetic network metrics that beat rgmet.
  prevWins <- 0    # Previous number of wins, i.e., the number to beat.
  metWinGoal <- 15 # Desired number of metrics to win

  start_time <- Sys.time()            # Get the current date and time.
  current_time <- Sys.time()
  workingUnits <- "hours"             # Change the working units to hours.
  end_time <- start_time + as.difftime(tLimit, units = workingUnits)
  print(paste("end time: ",end_time))

  # Convert real world sociomatrix to a network and calculate metrics.
    rwmat <- as.matrix(read.table(fnMat))

	rwnet <- igraph::graph_from_adjacency_matrix(rwmat,mode="undirected",weighted=TRUE)
  # Ensure that we are working with a symmetric matrix
	rwmat <- as.matrix(igraph::as_adjacency_matrix(rwnet,type="both"))
	rwnet <- igraph::graph_from_adjacency_matrix(rwmat,mode="undirected",weighted=TRUE)
	rwmet <- calculateMetrics(rwnet)

	rgmet <- avgRandomNetMet(rwmet[1],rwmet[4])

  # Metrics in rows, runs in columns
    metmat <- matrix(0,nrow=length(rwmet),ncol=iSampleSize)
  statdf <- data.frame(Evaluations=integer(),Wins=integer(),Losses=integer(),
                   Draws=integer(),Minutes=numeric(),Assignment=character())
  assignment <- matrix("",nrow=rwmet[1]) # Allocate memory for the cohort.
  t_r <- abs(rwmet - rgmet)              # RealWorld - RandomGraph metrics

  set.seed(1234)
  continueSearch <- TRUE
  while(continueSearch) {

    for (j in 1:iSampleSize) {
      # Use real world number of nodes to select personality types.
	  # Sample the compatibility table column names to produce a cohort.
     ## cohort <- as.matrix(sample(colnames(ct),rwmet[1], replace=TRUE))
		cohort <- replicate(rwmet[1],makeMBTI())
        indices <- match(cohort,colnames(ct)) # Get indices for look up table.
	  # Use current cohort to make a sociomatrix.
	    sm <- makeSociomatrix(indices,indices,ct,rwmet[4])
	  # Synthesize network from the sociomatrix.
        sn <- igraph::graph_from_adjacency_matrix(sm, mode="undirected")
	    snmet <- calculateMetrics(sn)
        metmat[,j] <- snmet                # Store metrics in a column.
		rm(indices, sm,sn)                 # Clean up for next iteration.
	} #next j
	cachet$synthNetMetMatrix <- metmat
	snAvgMet <- rowMeans(metmat) # Average across the rows of metrics matrix.
    t_s <- abs(rwmet - snAvgMet)
	## print(paste("t_s ",toString(t_s)))
	wins <- length(which(t_s < t_r))
	losses <- length(which(t_r < t_s))
	draws <- length(which(t_s == t_r))

##	assign("rngSeed",.Random.seed,envir=cachet)  # Store the seed for this result
	iTried <- get("evaluations", envir=cachet)  # get current total
	if (wins > prevWins) {
	   elapsed <- as.numeric(difftime(Sys.time(),start_time,units="mins"))
	   statdf <- rbind(statdf, data.frame(Evaluations=iTried,
	                              Wins=wins,Losses=losses,Draws=draws,
								  Minutes=round(elapsed,digits=2),
								  Assignment=toString(cohort)))
      ## print(paste("wins: ", wins, " evaluations: ", iTried))
	   write.table(statdf, file=fnStat, sep = ",", eol = "\n",row.names=FALSE)
	   assignment <- cohort
	   write.csv(assignment, file=fnOpt, eol="\n",row.names=FALSE)

	   rpt <- compileReport(rwmet,rgmet,snAvgMet)
	   write.csv(rpt, file=fnMet, eol="\n", row.names=FALSE)

	   mRNMet <- get("randomNetMetMatrix", envir=cachet)
	   write.csv(mRNMet, file=fnR, eol="\n", row.names=FALSE)
       mSNMet <- get("synthNetMetMatrix", envir=cachet)
	   write.csv(mSNMet, file=fnS, eol="\n", row.names=FALSE)
	   prevWins <- wins           # current score to compare to metWinGoal

	  }#endif

	  metmat <- matrix(0,nrow=length(rwmet),ncol=iSampleSize)  # Reset for the next iteration.
	  wins <- 0
	  losses <- 0
	  draws <- 0
	# Get the elapsed wall-clock time in seconds.
	  current_time <- Sys.time()

	   if (prevWins >= metWinGoal) { continueSearch <- FALSE  }
	   if (current_time >= end_time) { continueSearch <- FALSE }
   } #wend
 }

#' Compile a Report
#'
#' After calculating the metrics of a real world network, means of metrics
#' for a population of randomly generated networks, and means of metrics for
#' a population of personality-based synthesized networks, call this function
#' to compile the results into a report. The data returned by this function
#' can be used by the report table generator in this package.
#' @param rwmet Real world network metrics
#' @param mmet1 Mean metrics of a population of randomly generated networks.
#' @param mmet2 Mean metrics for compatibility synthesized network population.
#' @return A report suitable for the report table generator.
#' @export
#' @examples
#' sn <- realnetwork("sociomatrix.txt")  # Metrics from a real world network
#' ravgmet <- avgRandomNetMet(40,0.32)   # Mean metrics from random net pop.
#' savgmet <- socialnetSynthesizer(40,0.32) # Mean metrics
#' rpt <- compileReport(sn,ravgmet,savgmet)
compileReport <- function (rwmet,mmet1,mmet2) {
  metrics <- c("Number of nodes",  "Number of links", "Number of Components",
              "Network density",  "Degree average",  "Degree std.dev.",
			  "Cluster Coefficient", "Avg. Cluster Coefficient",
			  "Mean path length",  "Avg.Betweeness",   "Max.Betweeness",
			  "Avg.Closeness",     "Min.Closeness",    "Avg.Eigencentrality",
			  "Network radius","    Avg.Eccentricity", "Network diameter")
# Determine difference between the real world metrics and:
  tm_rm <- abs(rwmet - mmet1)  # mean metrics for population of random networks
  tm_sm <- abs(rwmet - mmet2)  # mean metrics for pop. of compatible synthnets
  rpt <- data.frame(metrics, rwmet, mmet2, tm_sm, mmet1, tm_rm)
  rpt <- format(rpt,digits=0,nsmall=3,format="f")
  return(rpt)
}

#' Generate a Report Table
#'
#' The report generator accepts a compiled report, which is produced by
#' another function in this package. This function uses the formatter and
#' format_table functions from the formattable package. The formatted table
#' returned from this function can be written to an HTML file and viewed
#' with a web browser.
#' @param rpt A compiled report with five columns of metrics
#' @param reportTitle The name of the real world social network
#' @return HTML code that can be written to a file
#' @export
#' @examples
#' sn <- realnetwork("sociomatrix.txt")  # Metrics from a real world network
#' ravgmet <- avgRandomNetMet(40,0.32)   # Mean metrics from random net pop.
#' savgmet <- socialnetSynthesizer(40,0.32) # Mean metrics
#' rpt <- compileReport(sn,ravgmet,savgmet)
#' dfrmTbl <- generateTable(rpt,"Social Network Name")
#' fileConn<-file("outputfileName.html")
#' writeLines(dfrmTbl, fileConn)
#' close(fileConn)
generateTable <- function(rpt,reportTitle) {
    colnames(rpt) <- c("Metrics","T<sub><i>x</i></sub>",
	                   "S<sub><i>x&#772;</i></sub>",
                       "|T<sub><i>x</i></sub>-S<sub><i>x&#772;</i></sub>|",
				       "R<sub><i>x&#772;</i></sub>",
					   "|T<sub><i>x</i></sub>-R<sub><i>x&#772;</i></sub>|")

   tblTitle <- paste("<h4>",reportTitle,"</h4>")

   bgf1 <- formattable::formatter("span", style =
         ~ formattable::style("background-color" = ifelse(
		  as.numeric(rpt[,'|T<sub><i>x</i></sub>-S<sub><i>x&#772;</i></sub>|'])
	    < as.numeric(rpt[,'|T<sub><i>x</i></sub>-R<sub><i>x&#772;</i></sub>|']),
	                                                   "SpringGreen","White")))
   bgf2 <- formattable::formatter("span", style =
         ~ formattable::style("background-color" = ifelse(
		   as.numeric(rpt[,'|T<sub><i>x</i></sub>-R<sub><i>x&#772;</i></sub>|'])
	     < as.numeric(rpt[,'|T<sub><i>x</i></sub>-S<sub><i>x&#772;</i></sub>|']),
	                                                     "SandyBrown","White")))
   frmtble  <- formattable::format_table(rpt,
                 list('|T<sub><i>x</i></sub>-S<sub><i>x&#772;</i></sub>|'=bgf1,
                     '|T<sub><i>x</i></sub>-R<sub><i>x&#772;</i></sub>|'=bgf2),
					                          align="rccccc", caption=tblTitle,
					             table.attr = "class=\"table table-bordered\"")
	return(frmtble)
 }
#' Generate an Algorithm Comparison Table
#'
#' The report generator accepts a compiled report, which is produced by
#' another function in this package. This function uses the formatter and
#' format_table functions from the formattable package. The formatted table
#' returned from this function can be written to an HTML file and viewed
#' with a web browser.
#' @param rpt A compiled report with five columns of metrics
#' @param reportTitle The name of the real world social network
#' @return HTML code that can be written to a file
#' @export
#' @examples
#' sn <- realnetwork("sociomatrix.txt")  # Metrics from a real world network
#' ravgmet <- avgRandomNetMet(40,0.32)   # Mean metrics from random net pop.
#' savgmet <- socialnetSynthesizer(40,0.32) # Mean metrics
#' rpt <- compileReport(sn,ravgmet,savgmet)
#' dfrmTbl <- generateTable(rpt,"Social Network Name")
#' fileConn<-file("outputfileName.html")
#' writeLines(dfrmTbl, fileConn)
#' close(fileConn)
generateComparisonTable <- function(rpt,reportTitle) {
   leftColor <- "lawngreen"
   rightColor <-  "lawngreen"
   trueMet <- "T<sub><i>x</i></sub>"
   mcAvgMet <- "MC(S<sub><i>x&#772;</i></sub>)"
   t_mcS <- "|T<sub><i>x</i></sub>-MC(S<sub><i>x&#772;</i></sub>)|"
   gaAvgMet <- "GA(S<sub><i>x&#772;</i></sub>)"
   t_gaS <-  "|T<sub><i>x</i></sub>-GA(S<sub><i>x&#772;</i></sub>)|"

   colnames(rpt) <- c("Metrics", trueMet,mcAvgMet,t_mcS,gaAvgMet,t_gaS)

   tblTitle <- paste("<h4>",reportTitle,"</h4>")
   togCol4 <- as.matrix(rpt[,4]) < as.matrix(rpt[,6])
   togCol6 <- as.matrix(rpt[,6]) < as.matrix(rpt[,4])

   bgf1 <- formattable::formatter("span", style =
         ~ formattable::style("background-color" = ifelse(togCol4,
		                                           leftColor,"White")))
   bgf2 <- formattable::formatter("span", style =
         ~ formattable::style("background-color" = ifelse(togCol6,
		                                           rightColor,"White")))
   frmtble  <- formattable::format_table(rpt,
           list('|T<sub><i>x</i></sub>-MC(S<sub><i>x&#772;</i></sub>)|' = bgf1,
			    '|T<sub><i>x</i></sub>-GA(S<sub><i>x&#772;</i></sub>)|' = bgf2),
				align="rccccc", caption=tblTitle,
					              table.attr = "class=\"table table-bordered\"")
	return(frmtble)
 }
#' Compile Algorithm Comparison Report
#'
#' After generating reports that compare the search algorithms with the
#' randomly generated networks, this function compiles a data.frame that
#' compares the metrics resulting from personality assignments found by
#' the two search algorithms.
#' @param fnMC A file name of a Monte Carlo search results report
#' @param fnGA A file name of a Genetic Algorithm search results report
#' @return HTML code that can be written to a file
#' @export
#' @examples
#' fnMC <- "../reports/mcRawReport_RobinsBank.txt"
#' fnGA <- "../reports/gaWeaselRawReport_RobinsBank.txt"
#' compileSearchComparisonReport(fnMC,fnGA)
compileSearchComparisonReport <-  function(fnMC,fnGA,reportTitle) {
   rptMC <- as.data.frame(read.table(fnMC))
   rptGA <- as.data.frame(read.table(fnGA))
   rptIt <- cbind(as.character(rptMC[,1]),rptMC[,2],rptMC[,3],rptMC[,4],
                                                   rptGA[,3],rptGA[,4])
   rptIt <- as.data.frame(rptIt)
   compReport <- generateComparisonTable(rptIt,reportTitle)
   return(compReport)
}


#' Genetic Algorithm Search for Optimum Network Metrics
#'
#' Using a real world sociomatrix for comparison, this evolutionary 
#' algorithm produces an intial population of sociomatrices and compares
#' the metrics with the metrics of network generated from the real world
#' sociomatrix. A fitness function compares the difference between the
#' metrics and returns a score. 
#' @param rwmat A real world sociomatrix
#' @param ct A personality compatibility look up table
#' @param fnStat A file name for a status report from this search function
#' @param fnOpt A text file with a matrix with the optimum set of personalities
#' @return This function does not return anything
#' @export
#' @examples
#' cpat <- as.matrix(read.csv("CompatibilityTable.csv")
#' rbmet <- calculateMetrics(rb)      
#' rgmet <- avgRandomNetMet(rbmet[1],rbmet[4])
#' gaSearch(1000,"Robins_Bank.txt",cpat,"gaStat.csv","GA_OptSet.csv")
gaMetricsSearch <- function(iSampleSize,iPopSize,fnMat,fnMet, ct,fnStat,
                                                       fnOpt,fnR,fnS,tLimit) {
    mutateProb <- 0.2      # Probability of a mutation
    highscore <- 0         # Initialize high score.
	similarity <- 0        # Percentage of similarity
	genNum <- 0            # Generation number
	metWinGoal <- 15       # Desired number of metrics to win
	snAvgMet <- matrix(0,nrow=17,ncol=1)
	
	set.seed(1234)         # Initialize random number generator.
	
    start_time <- Sys.time()            # Get the current date and time.
	current_time <- Sys.time()
	workingUnits <- "hours"              # Change the working units to hours.
    end_time <- start_time + as.difftime(tLimit, units = workingUnits)
	print(paste("end time: ",end_time))
  # Convert real world sociomatrix to a network and calculate metrics.
    rwmat <- as.matrix(read.table(fnMat))
	rwnet <- igraph::graph_from_adjacency_matrix(rwmat,mode="undirected",weighted=TRUE)
  # Ensure that we are working with a symmetric matrix
	rwmat <- as.matrix(igraph::as_adjacency_matrix(rwnet,type="both"))
	rwnet <- igraph::graph_from_adjacency_matrix(rwmat,mode="undirected",weighted=TRUE)	
	rwmet <- calculateMetrics(rwnet)
	
    numNodes <- rwmet[1]      # number of nodes
	goaldensity <- rwmet[4]   # get the real world network density

  # Replace any zeros with ones to prevent a divide by zero.
	rwmet2 <- sapply(rwmet,function(x) if(x == 0) {x = 1} else { x = x })
  # Calculate averaged metrics of random graphs for goal setting.
	rgmet <- avgRandomNetMet(rwmet[1],rwmet[4])
	metGoals <- setMetricGoals(rwmet,rgmet)

	spawnRecord <- function(xmethod, cohort,gendf) { 
   # Metrics in rows, runs in columns
     metmat <- matrix(0,nrow=length(rwmet),ncol=iSampleSize)  
	 for (j in 1:iSampleSize) {
	   indices <- match(cohort,colnames(ct)) # Get indices for look up table.
	  # Use current cohort to make a sociomatrix.
		sm <- makeSociomatrix(indices,indices,ct,goaldensity)
		sn <- igraph::graph_from_adjacency_matrix(sm, mode="undirected")
		snmet <- calculateMetrics(sn)
        metmat[,j] <- snmet                # Store metrics in a column.
		rm(indices, sm,sn)                 # Clean up for next iteration.
	} #next j
	cachet$synthNetMetMatrix <- metmat
	# Average across the rows of metrics matrix.
	  snAvgMet <<- rowMeans(metmat)  
      goodness <- 1 - abs(rwmet2 - snAvgMet) / rwmet2
##  fitness <- sapply(goodness,function(x)if(x >desiredQuality){x = 1} else {x = 0})
	  fitness <- goodness > metGoals   # ought to be Booleans
	  score <- sum(fitness)
	  
      if(score > highscore) {
          # Store the seed for this result; this helps with reproducibility.
		  ## assign("gaSeed",.Random.seed,envir=cachet)  
		print(paste("goodness: ",goodness[length(goodness)],
		            "fitness :",fitness[length(fitness)],"score :",score ))
		   highscore <<- score           # super assign to parent global.
		   similarity <<- 1 - abs(sum(rwmet) - sum(snAvgMet)) / sum(rwmet)
		 # Store the best mean metrics in a file for use by the report generator.
           write.csv(snAvgMet, file=fnMet, eol="\n", row.names=FALSE)	
	   # Record fields: score,similarity,evaluations,mateMethod,cohort
	      iTried <- cachet$evaluations
		  gendf[nrow(gendf)+1,] <- c(score, similarity, iTried, xmethod, 
		                                     toString(cohort),toString(snAvgMet))
         }  
		## print(gendf[nrow(gendf),])
		 return(gendf)
   }
  #------- Mutation and Cross Over Methods ----------------------
   mutate <- function(chromosome,geneSet, numChanges) {
    # Receive a chromosome, geneSet, probability of change, and number
	# of potential changes.
      mutatedAssignment <- chromosome
	  impact <- sample(1:numChanges,1)
      for(i in 1:impact) {
         geneNum <- sample(1:length(chromosome),1)
	 ##  mutatedAssignment[geneNum] <- geneSet[sample(1:length(geneSet),1)]
		 mutatedAssignment[geneNum] <- makeMBTI()
      } #next
        return(mutatedAssignment)
   }
   xOnePoint <- function(adam,eve) {       # Child receives first half of genes
     ihalf <- floor(length(eve) * 0.5)     # from the father and the second
	 vc1stHalf <- adam[1:ihalf]            # half from the mother.
	 iUpperHalf <- ihalf + 1
     vc2ndHalf <- eve[iUpperHalf:length(eve)]
     vc <- c(vc1stHalf,vc2ndHalf)
	 return(vc)
   }
   xTwoPoint <- function(adam,eve) {         # First third of genes from father
      iLo3rd <- ceiling(length(eve) * 0.33)  # Middle third from the mother
	  iHi3rd <- floor(length(eve) * 0.66)    # Final third are from the father
	  iLowerThird <- iLo3rd - 1
	  iUpperThird <- iHi3rd + 1
	  vcLoThird <- adam[1:iLowerThird]
	  vcMiddle <- eve[iLo3rd:iHi3rd]
	  vcHiThird <- adam[iUpperThird:length(adam)]
	  
	  vc <- c(vcLoThird,vcMiddle,vcHiThird)
	  return(vc)
   }
   xCrissCross <- function(adam,eve) {      # First half of the genes are from
     ihalf <- floor(length(eve) * 0.5)      # second half of genes from mother
	 vc1Half <- adam[1:ihalf]               # Second half of genes are from the
	 upperHalf <- ihalf + 1
     vc2Half <- eve[upperHalf:length(eve)]  # first half of genes from father
     vc <- c(vc2Half,vc1Half)
	 return(vc)
     }
	 
   xAlternate <- function(adam,eve) { # Odd genes from father, even from mother
    # The algorithm for this code was found at 
	# http://r.789695.n4.nabble.com/Interleaving-elements-of-two-vectors-td795123.html
     vcOdds <- adam[seq(from=1,to=length(adam),by=2)]
     vcEvens <- eve[seq(from=2,to=length(eve),by=2)]
     minlen <- min(length(vcOdds),length(vcEvens))
     minseq <- seq(length=minlen)
     vc <- c(rbind(vcOdds[minseq],vcEvens[minseq]),
	               vcOdds[-minseq],vcEvens[-minseq])
	 return(vc)
   }
 
   firstGen <- function() {
     # Establish headers for a data frame to hold the population.	 
       gen <- data.frame(score=integer(), similarity=numeric(), 
	                     evaluations=integer(), mateMethod=vector(), 
						 cohort=vector(), metrics=vector())
	
        for(i in 1:iPopSize) {
       ## cohort <- sample(colnames(ct),numNodes, replace=TRUE)
		  cohort <- replicate(numNodes,makeMBTI())
          gen <- spawnRecord("None", cohort,gen)
        }# next i
		
		# Rank order the population by score.
        gen <- gen[order(as.numeric(gen$score),decreasing = TRUE),]
  	   ##  print("Done with first generation")
		genNum <<- genNum + 1
		return(gen)
   }
   nextGen <- function(old_gen) {
     # Establish headers for a data frame to hold the population.	 
       gen <- data.frame(score=integer(), similarity=numeric(), 
	                     evaluations=integer(), mateMethod=vector(), 
						 cohort=vector(), metrics=vector())
	   gen[nrow(gen)+1,] <- old_gen[1,]  # Store the last gen's best design.
	   gen[nrow(gen)+1,] <- old_gen[2,]  # Store last gen's second best design.
	   vA <- unlist(strsplit(old_gen$cohort[1],", "))
	   vB <- unlist(strsplit(old_gen$cohort[2],", "))
	   rm(old_gen)   # Clear some workspace.
	   
	   impact <- ceiling(length(vA) * 0.5) # potential number of mutations
	   numMatingMethods <- 4                # current number of methods
	   matingMethod <- 1                    # starting method
	   xMethod <- "Mutation"
	   for(i in 3:iPopSize) {
	     if(runif(1) <= mutateProb) {
		   design <- mutate(vA,colnames(ct), impact)
		 } else {
		   xMethod <- switch(matingMethod,"SinglePoint","TwoPoint",
		                                   "CrissCross","Alternate")
		   design <- switch(matingMethod, xOnePoint(vA,vB),
		                                  xTwoPoint(vA,vB),
										  xCrissCross(vA,vB),
										  xAlternate(vA,vB))
		 }
	     gen <- spawnRecord(xMethod,design,gen)
		 
		 if(matingMethod < numMatingMethods) { 
		     matingMethod <- matingMethod + 1
		} else {
		     matingMethod <- 1
		}
	   } #next
	   
	  # Rank order the population by score.
        gen <- gen[order(as.numeric(gen$score),decreasing = TRUE),]
		genNum <<- genNum + 1
	 ##  print(paste("Completed generation ",genNum))
       return(gen)
   }

  #--------- Main --------------+
   statusReport <- data.frame(genNum=integer(),score=integer(), similarity=numeric(), 
	                     evaluations=integer(), mateMethod=vector(), minutes=numeric(),
						 cohort=vector())
    prevGen <- firstGen()
	
	lastscore <- 0	
	continueSearch <- TRUE
	while(continueSearch) {  
	
	 # Record the best chromosome from the previous generation.
	  if(highscore > lastscore) {
	   ##  print(paste("Generation number: ",genNum)) 
		 elapsedTime <- as.numeric(difftime(Sys.time(),start_time),units="mins")
		 statusReport[nrow(statusReport)+1,] <- c(genNum, highscore,
		                                         prevGen[1,]$similarity, cachet$evaluations,
												 prevGen[1,]$mateMethod, 
												 round(elapsedTime,digits=2),
												 prevGen[1,]$cohort)		 
	  ##   statusReport[nrow(statusReport)+1,] <- prevGen[1,]
		 write.table(statusReport, file=fnStat, sep = ",", eol = "\n",row.names=FALSE)
		 
	     assignment <- as.matrix(unlist(strsplit(statusReport$cohort[nrow(statusReport)],", ")))
	     write.csv(assignment, file=fnOpt, eol="\n",row.names=FALSE)	 
		 
		 snmet <- as.numeric(unlist(strsplit(prevGen[1,]$metrics,", ")))
		 rpt <- compileReport(rwmet,rgmet,snmet)
	     write.csv(rpt, file=fnMet, eol="\n", row.names=FALSE)
		 
		 mRNMet <- get("randomNetMetMatrix", envir=cachet)
	     write.csv(mRNMet, file=fnR, eol="\n", row.names=FALSE)
         mSNMet <- get("synthNetMetMatrix", envir=cachet)
	     write.csv(mSNMet, file=fnS, eol="\n", row.names=FALSE)
		 
		 lastscore <- highscore
	   }
	   currentGen <- nextGen(prevGen)
	   prevGen <- currentGen
	# Get the elapsed wall-clock time.
	  current_time <- Sys.time()
	  
	   if (lastscore == highscore & lastscore >= metWinGoal) { continueSearch <- FALSE  }
	   if (current_time >= end_time) { continueSearch <- FALSE }
	}
 }
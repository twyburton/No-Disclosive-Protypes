
ds.meanPlot <- function( table=NULL , x.col=NULL , y.col=NULL , type='combine', grid.dim=10 , recursiveMode=TRUE ) {

	# if no opal login details are provided look for 'opal' objects in the environment
##	if(is.null(datasources)){
##		datasources <- findLoginObjects()
##	}


# SERVER CALL STUFF
	output.local <- meanPlotDS( table , x.col , y.col , grid.dim , recursiveMode )


	if(type=="combine"){

		plot(output.local[,x.col] , output.local[,y.col], xlab = x.col, ylab = y.col, col="black")		
		print(paste( "Points Plotted: "  , nrow(output.local)))

	}else if(type=="split"){
    
		
  
	}else{
		stop('Function argument "type" has to be either "combine" or "split"')
	}


}


###### TESTING ######

data <- read.csv("O:/Documents/Data/New_HOP_Data/HOP_simulated_data.csv", header=TRUE )

system.time( output <- ds.meanPlot( data[1:10000,] , x.col="LAB_HDL" , y.col="LAB_TSC" , grid.dim=10 , recursiveMode=TRUE ) )
	




distance <- function( x0 , y0 , x1 , y1 ){
	return (sqrt( abs( (x0 - x1)^2 +(y0 - y1)^2 ) ))
}

closest4 <- function( table , xCol , yCol ) {

	x.save <- rep(NA,length=100 )
	y.save <- rep(NA,length=100 )
	g.save <- rep(NA,length=100 )



	## save original table for later
	original_table = table

	## Group size
	group_size = 5

	## Initalised output dataframe
	colClasses = c( typeof(table[, xCol]) , typeof(table[, yCol])  )
	col.names = c( xCol, yCol )

	closestOutput <- read.table(text = "",
                 colClasses = colClasses,
                 col.names = col.names)


	## Standardise scale
	table[,xCol] <- (table[,xCol] - mean(table[,xCol]) )/sqrt(var(table[,xCol]))
	table[,yCol] <- (table[,yCol] - mean(table[,yCol]) )/sqrt(var(table[,yCol]))

	table <- table[, c( xCol, yCol ) ]

	## Get min values
	xMin <- min(table[, xCol ], na.rm = TRUE)
	yMin <- min(table[, yCol ], na.rm = TRUE)
	xMax <- max(table[, xCol ], na.rm = TRUE)
	yMax <- max(table[, yCol ], na.rm = TRUE)
	
	xDelta <- ( xMax - xMin ) * 0.5
	yDelta <- ( yMax - yMin ) * 0.5

	xDiff <- ( xMax - xMin ) + ( xMax - xMin ) * 0.01
	yDiff <- ( yMax - yMin ) + ( yMax - yMin ) * 0.01
	
	## Split table into array of dataframes
	n_xGrids = 10
	n_yGrids = 10
	gridWidth = xDiff/n_xGrids
	gridHeight = yDiff/n_yGrids

		# Init array of dataframes
	#gridArray <- rep(NA, n_xGrids * n_yGrids)
	gridArray <- data.frame(rep(NA, n_xGrids * n_yGrids))
	print(gridArray)


	##gridArray <- matrix( rep(NA, n_xGrids * n_yGrids), 1, n_xGrids * n_yGrids )
	
	print(xMin)
	print(yMin)
	print(gridWidth)
	print(gridHeight)

	for( i in 1:nrow(table)){ 
		xGrid = floor( ( table[i,xCol] - xMin ) / gridWidth ) + 1
		yGrid = floor( ( table[i,yCol] - yMin ) / gridHeight ) + 1

		gridIndex = xGrid + ( yGrid * n_xGrids ) - n_xGrids

		print( gridIndex )
		print( xGrid )
		print( yGrid )

		if( is.na( gridArray[ gridIndex ] )  ){
			

			gridArray[ gridIndex, 1 ] <- data.frame( table[i,xCol] , table[i,yCol] )
			print( typeof( gridArray[ gridIndex ] ))
			print( gridArray[ gridIndex ] )
		} else {
			print("NOT NULL")
			print(gridArray[gridIndex])
			gridArray[ gridIndex, ][ nrow(gridArray[gridIndex]) + 1 , ] <- c(  table[i,xCol], table[i,yCol] )


			#gridArray[ gridIndex ] <- rbind(  table[i,xCol], table[i,yCol] )

		}

	}




	for( i in 1:nrow(table)){ 
		
		if( i %% 500 == 0 ) {
			print(paste0(i," Out Of ", nrow(table), " Calculated"))
		}

		xPos <- table[i,xCol]
		yPos <- table[i,yCol]

		smallBox <- table[ 	  ( table[,xCol] < xPos + xDelta)
						& ( table[,xCol] > xPos - xDelta)
						& ( table[,yCol] < yPos + yDelta)
						& ( table[,yCol] > yPos - yDelta)
						& ( distance( table[,xCol] , table[,yCol] , xPos , yPos ) != 0 )
					, ]
		
	
		if( nrow(smallBox) < 4 ){
			print("Error")
			break
		} else {
			smallBox[,"distance"] <-  distance( xPos, yPos, smallBox[,xCol] , smallBox[,yCol])
			subSmall <- smallBox[ order(smallBox[,"distance"]), ][1:4,]

			# Reset row indexes
			rownames(subSmall) <- 1:nrow(subSmall)

			subSmall[ nrow(subSmall) + 1, ] <- c( xPos , yPos, 0 )
			avg <- colMeans( subSmall)

			closestOutput[ nrow(closestOutput) + 1, ] <- c( avg[ xCol ] , avg[ yCol ] )
			plot( closestOutput[,xCol] , closestOutput[,yCol], xlab = xCol, ylab = yCol, col="black")	
			points( subSmall[,xCol], subSmall[,yCol] , col="red")

		}
	
	}

	plot( original_table[,xCol], original_table[,yCol], xlab = xCol, ylab = yCol, col="red")	


	## Undo standardised scale
	closestOutput[,xCol] <- (closestOutput[,xCol] * sqrt(var(original_table[,xCol]))) + mean(original_table[,xCol])
	closestOutput[,yCol] <- (closestOutput[,yCol] * sqrt(var(original_table[,yCol]))) + mean(original_table[,yCol])

	points( closestOutput[,xCol] , closestOutput[,yCol] , col="green")

	#plot( table[,xCol], table[,yCol], xlab = xCol, ylab = yCol )


}



data <- read.csv("O:/Documents/Data/New_HOP_Data/HOP_simulated_data.csv", header=TRUE )  # read csv file 
system.time( closest4( data[1:100,] , "LAB_HDL" , "LAB_TSC" ) , TRUE )

#data = data.frame( 1:10 , 1:10 )
#system.time( closest4( data , 1 , 2 ) , TRUE )


library("parallel")
library("foreach")
library("doParallel")


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
	min_group_size = 5

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
	n_xGrids <- 10
	n_yGrids <- 10
	gridWidth <- xDiff/n_xGrids
	gridHeight <- yDiff/n_yGrids

	# Init array of dataframes
	gridList <- vector()
	length(gridList) <- n_xGrids * n_yGrids

	
	print(xMin)
	print(yMin)
	print(gridWidth)
	print(gridHeight)

	print( paste("Sorting ", nrow(table), " Points Into ", n_xGrids * n_yGrids, " Cells" ))

	for( i in 1:nrow(table)){ 
		xGrid <- floor( ( table[i,xCol] - xMin ) / gridWidth ) + 1
		yGrid <- floor( ( table[i,yCol] - yMin ) / gridHeight ) + 1

		gridIndex = xGrid + ( yGrid * n_xGrids ) - n_xGrids

		#print( gridIndex )
		#print( xGrid )
		#print( yGrid )

		if( is.na( gridList[ gridIndex ] )  ){
			gridList[ gridIndex ] <- list( i )	
		} else {
			gridList[[ gridIndex ]][ length(gridList[[ gridIndex ]]) + 1 ] <- i
		}

	}


	print("Points Sorted Into Cells")

	cell_count_to_small = 0
	## go through each cell and find 5 closest
	for( i in 1:(n_xGrids * n_yGrids) ){
		if( length( gridList[[i]] ) < min_group_size ){
			cell_count_to_small = cell_count_to_small + 1
		} else {

			cells_to_compare <- c( i , i - 1, i + 1, i - n_xGrids, i - n_xGrids + 1 , i - n_xGrids - 1, i + n_xGrids, i + n_xGrids + 1, i + n_xGrids - 1 )
			points_to_check <- unlist(gridList[ cells_to_compare ])
		
			print(paste("Cell ", i , " " , length(points_to_check), " To Check" ))

			table_to_check <- table[points_to_check,] 
			table_points_to_do <- table[ unlist(gridList[i]), ]

			for( j in 1:length( unlist( gridList[i] ) )  ){
				xPos = table_points_to_do[j, xCol ]
				yPos = table_points_to_do[j, yCol ]

				#print(table_to_check[,xCol])

				smallBox<- table_to_check[ ( distance( table_to_check[,xCol] , table_to_check[,yCol] , xPos , yPos ) != 0) ,]

				smallBox[,"distance"] <-  distance( xPos, yPos, smallBox[,xCol] , smallBox[,yCol])
				subSmall <- smallBox[ order(smallBox[,"distance"]), ][1:4,]

				# Reset row indexes
				rownames(subSmall) <- 1:nrow(subSmall)

				subSmall[ nrow(subSmall) + 1, ] <- c( xPos , yPos, 0 )
				avg <- colMeans( subSmall)

				closestOutput[ nrow(closestOutput) + 1, ] <- c( avg[ xCol ] , avg[ yCol ] )
				#plot( closestOutput[,xCol] , closestOutput[,yCol], xlab = xCol, ylab = yCol, col="black")	
				#points( subSmall[,xCol], subSmall[,yCol] , col="red")

			}

			plot( closestOutput[,xCol] , closestOutput[,yCol], xlab = xCol, ylab = yCol, col="black")	
		}

		
	} 



#	for( i in 1:nrow(table)){ 
#		
#		if( i %% 500 == 0 ) {
#			print(paste0(i," Out Of ", nrow(table), " Calculated"))
#		}
#
#		xPos <- table[i,xCol]
#		yPos <- table[i,yCol]
#
#		smallBox <- table[ 	  ( table[,xCol] < xPos + xDelta)
#						& ( table[,xCol] > xPos - xDelta)
#						& ( table[,yCol] < yPos + yDelta)
#						& ( table[,yCol] > yPos - yDelta)
#						& ( distance( table[,xCol] , table[,yCol] , xPos , yPos ) != 0 )
#					, ]
#		
#	
#		if( nrow(smallBox) < 4 ){
#			print("Error")
#			break
#		} else {
#			smallBox[,"distance"] <-  distance( xPos, yPos, smallBox[,xCol] , smallBox[,yCol])
##			subSmall <- smallBox[ order(smallBox[,"distance"]), ][1:4,]
#
#			# Reset row indexes
#			rownames(subSmall) <- 1:nrow(subSmall)
#
#			subSmall[ nrow(subSmall) + 1, ] <- c( xPos , yPos, 0 )
#			avg <- colMeans( subSmall)
#
#			closestOutput[ nrow(closestOutput) + 1, ] <- c( avg[ xCol ] , avg[ yCol ] )
#			plot( closestOutput[,xCol] , closestOutput[,yCol], xlab = xCol, ylab = yCol, col="black")	
#			points( subSmall[,xCol], subSmall[,yCol] , col="red")
#
#		}
#	
#	}

	#plot( original_table[,xCol], original_table[,yCol], xlab = xCol, ylab = yCol, col="red")	


	## Undo standardised scale
	closestOutput[,xCol] <- (closestOutput[,xCol] * sqrt(var(original_table[,xCol]))) + mean(original_table[,xCol])
	closestOutput[,yCol] <- (closestOutput[,yCol] * sqrt(var(original_table[,yCol]))) + mean(original_table[,yCol])

	plot(closestOutput[,xCol] , closestOutput[,yCol], xlab = xCol, ylab = yCol, col="green")	

	print(paste( "Points Plotted: "  , nrow(closestOutput)))

	#plot( table[,xCol], table[,yCol], xlab = xCol, ylab = yCol )

	closestOutput <<- closestOutput

}



data <- read.csv("O:/Documents/Data/New_HOP_Data/HOP_simulated_data.csv", header=TRUE )  # read csv file 
system.time( closest4( data[1:10000,] , "LAB_HDL" , "LAB_TSC" ) , TRUE )

#data = data.frame( 1:10 , 1:10 )
#system.time( closest4( data , 1 , 2 ) , TRUE )

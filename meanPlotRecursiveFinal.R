distance <- function( x0 , y0 , x1 , y1 ){
	return (sqrt( abs( (x0 - x1)^2 +(y0 - y1)^2 ) ))
}

meanPlot <- function( table=NULL , xCol=NULL , yCol=NULL , gridDim=10 , recursiveMode=TRUE ) {

	## Save original table for later - Required for undoing standardised scale
	original_table = table

	## Group size
	min_group_size = 5

	## Recusive threshold 
	##	- If recursiveMode = TRUE this is the cell value at which a meanPlot will be recursivly called onto a cell
	recusive_threshold = 500

	## Initalised output dataframe
	colClasses = c( typeof(table[, xCol]) , typeof(table[, yCol])  )
	col.names = c( xCol, yCol )

	closestOutput <- read.table(text = "",
                 colClasses = colClasses,
                 col.names = col.names)

	## Remove unneeded columns from table
	table <- table[, c( xCol, yCol ) ]

	## Standardise scale
	table[, xCol ] <- (table[, xCol ] - mean(table[, xCol] ) )/sqrt(var(table[, xCol ]))
	table[, yCol ] <- (table[, yCol ] - mean(table[, yCol] ) )/sqrt(var(table[, yCol ]))

	## Get min values
	xMin <- min(table[, xCol ], na.rm = TRUE)
	yMin <- min(table[, yCol ], na.rm = TRUE)
	xMax <- max(table[, xCol ], na.rm = TRUE)
	yMax <- max(table[, yCol ], na.rm = TRUE)

	xDiff <- ( xMax - xMin ) + ( xMax - xMin ) * 0.01
	yDiff <- ( yMax - yMin ) + ( yMax - yMin ) * 0.01
	
	## Split table into array of dataframes
	n_xGrids <- gridDim
	n_yGrids <- gridDim
	gridWidth <- xDiff/n_xGrids
	gridHeight <- yDiff/n_yGrids

	# Init vector of grid indexs
	gridList <- vector()
	length(gridList) <- n_xGrids * n_yGrids

	print( paste("Sorting ", nrow(table), " Points Into ", n_xGrids * n_yGrids, " Cells" ))

	## ==== Go through each point of data and sort into the appropriate grid cell ====
	for( i in 1:nrow(table)){ 
		xGrid <- floor( ( table[i, xCol ] - xMin ) / gridWidth ) + 1
		yGrid <- floor( ( table[i, yCol ] - yMin ) / gridHeight ) + 1

		gridIndex = xGrid + ( yGrid * n_xGrids ) - n_xGrids

		if( is.na( gridList[ gridIndex ] )  ){
			gridList[ gridIndex ] <- list( i )	
		} else {
			gridList[[ gridIndex ]][ length(gridList[[ gridIndex ]]) + 1 ] <- i
		}
	}



	## ==== Points have been sorted into their cells ====
	## ==== Go through each grid cell and find closest ====

	cell_count_to_small = 0
	## Iterate through each cell
	for( i in 1:(n_xGrids * n_yGrids) ){

		## If the number of point in the cell is less than the disclosive value then discard the cell.
		if( length( gridList[[i]] ) < min_group_size ){
			cell_count_to_small = cell_count_to_small + 1

		} else {

			## If the number of points in the cell is less than the recursive threshold or not 
			##	in recursive mode perform closest calculation on cell
			if( length( gridList[[i]] ) < recusive_threshold || !recursiveMode ){

				## Calculate with cells need to be checked for closest 4
				cells_to_compare <- c( i , i - 1, i + 1, i - n_xGrids, i - n_xGrids + 1 , i - n_xGrids - 1, i + n_xGrids, i + n_xGrids + 1, i + n_xGrids - 1 )
			
				## remove cells from outside boundry and form list of cells to check
				points_to_check <- unlist(gridList[ cells_to_compare[cells_to_compare[] >= 0 & cells_to_compare[] <= n_xGrids * n_yGrids ] ])
			
				print(paste("Cell ", i , " " , length(points_to_check), " To Check" ))

				table_to_check <- table[points_to_check,] 
				table_points_to_do <- table[ unlist(gridList[i]), ]

				## Go through each point in the inner cell and calculate nearest 4 mean.
				for( j in 1:length( unlist( gridList[i] ) )  ){
					## Get x and y for point to calculate
					xPos = table_points_to_do[j, xCol ]
					yPos = table_points_to_do[j, yCol ]

					## Get a list of all points to compare against
					smallBox<- table_to_check[ ( distance( table_to_check[,xCol] , table_to_check[,yCol] , xPos , yPos ) != 0) ,]

					## Calculate the distance to each point to compare against
					smallBox[,"distance"] <-  distance( xPos, yPos, smallBox[,xCol] , smallBox[,yCol])
		
					## Order the distances to find the closest 4
					subSmall <- smallBox[ order(smallBox[,"distance"]), ][1:4,]

					## Reset row indexes
					rownames(subSmall) <- 1:nrow(subSmall)

					## Create a table of just 5 points to calculate the mean for
					subSmall[ nrow(subSmall) + 1, ] <- c( xPos , yPos, 0 )
	
					## Calculate the x and y means
					avg <- colMeans( subSmall)

					## Add mean value to output table
					closestOutput[ nrow(closestOutput) + 1, ] <- c( avg[ xCol ] , avg[ yCol ] )
				}

			} else {
				closestOutput <- rbind( closestOutput , meanPlot( table[ unlist(gridList[i]), ] , xCol , yCol , ceiling(gridDim/2)) )
			}

		}

		
	} 


	print(paste(cell_count_to_small, " Cells To Small To Return" ))

	## Undo standardised scale
	closestOutput[,xCol] <- (closestOutput[,xCol] * sqrt(var(original_table[,xCol]))) + mean(original_table[,xCol])
	closestOutput[,yCol] <- (closestOutput[,yCol] * sqrt(var(original_table[,yCol]))) + mean(original_table[,yCol])


	## Return table of output values 
	return( closestOutput )




}


###### TESTING ######

data <- read.csv("O:/Documents/Data/New_HOP_Data/HOP_simulated_data.csv", header=TRUE )

system.time( output <- meanPlot( data[1:10000,] , "LAB_HDL" , "LAB_TSC" , 10 , TRUE ) )
	
plot(output[,"LAB_HDL"] , output[,"LAB_TSC"], xlab = "LAB_HDL", ylab = "LAB_TSC", col="black")		
print(paste( "Points Plotted: "  , nrow(output)))


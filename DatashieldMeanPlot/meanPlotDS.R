#' 
#' @title Creates a dataframe of mean points
#' @description The mean values of every point and its four closest neigbours is calculated for use by ds.meanPlot
#' @details Grid cells with less than 5 point are discarded.
#' @param x.col the name of the column to plot on the x-axis
#' @param y.col the name of the column to plot on the y-axis
#' @param grid.dim the number of cells that the x-axis and y-axis are split into
#' @param recursiveMode a logical expression for whether or not recursive mode is used during calcualtion
#' If \code{recursiveMode} is set to "TRUE", cells that contain more values than the recursive threshold will have meanPlotDS performed on them again. This significaly improves the time of the function.
#' If \code{recursiveMode} is set to "FALSE",
#' @return a dataframe containing the mean points
#' @author Burton, T.
#' @export
#' 


meanPlotDS <- function( x=NULL , y=NULL , grid.dim=10 , recursiveMode=TRUE ) {

####### TESTING DATA

	d <- read.csv("O:/Documents/Data/New_HOP_Data/HOP_simulated_data.csv", header=TRUE )

####### END TESTING DATA

	table <- data.frame( x , y )
	colnames(table) <- c( "x" , "y" )

	x.col <- "x"
	y.col <- "y"

	# Save original table for later - Required for undoing standardised scale
	table.save = table

	# Group size
	min_group_size = 5

	# Recusive threshold
	#	- If recursiveMode = TRUE this is the cell value at which a meanPlot will be recursivly called onto a cell
	recusive.threshold = 500

	# Initalised output dataframe
	colClasses = c( typeof(table[, x.col]) , typeof(table[, y.col])  )
	col.names = c( x.col, y.col )

	closestOutput <- read.table(text = "",
                 colClasses = colClasses,
                 col.names = col.names)

	# Remove unneeded columns from table
	table <- table[, c( x.col, y.col ) ]

	# Standardise scale
	table[, x.col ] <- (table[, x.col ] - mean(table[, x.col] ) )/sqrt(var(table[, x.col ]))
	table[, y.col ] <- (table[, y.col ] - mean(table[, y.col] ) )/sqrt(var(table[, y.col ]))

	# Get min values
	x.min <- min(table[, x.col ], na.rm = TRUE)
	y.min <- min(table[, y.col ], na.rm = TRUE)
	x.max <- max(table[, x.col ], na.rm = TRUE)
	y.max <- max(table[, y.col ], na.rm = TRUE)

	xDiff <- ( x.max - x.min ) + ( x.max - x.min ) * 0.01
	yDiff <- ( y.max - y.min ) + ( y.max - y.min ) * 0.01
	
	# Split table into array of dataframes
	n.xGrids <- grid.dim
	n.yGrids <- grid.dim
	gridWidth <- xDiff/n.xGrids
	gridHeight <- yDiff/n.yGrids

	# Init vector of grid indexs
	gridList <- vector()
	length(gridList) <- n.xGrids * n.yGrids

	# ==== Go through each point of data and sort into the appropriate grid cell ====
	for( i in 1:nrow(table)){ 
		x.grid <- floor( ( table[i, x.col ] - x.min ) / gridWidth ) + 1
		y.grid <- floor( ( table[i, y.col ] - y.min ) / gridHeight ) + 1

		gridIndex = x.grid + ( y.grid * n.xGrids ) - n.xGrids

		if( is.na( gridList[ gridIndex ] )  ){
			gridList[ gridIndex ] <- list( i )	
		} else {
			gridList[[ gridIndex ]][ length(gridList[[ gridIndex ]]) + 1 ] <- i
		}
	}

	# ==== Points have been sorted into their cells ====
	# ==== Go through each grid cell and find closest ====

	cell_count_to_small = 0
	# Iterate through each cell
	for( i in 1:(n.xGrids * n.yGrids) ){

		# If the number of point in the cell is less than the disclosive value then discard the cell.
		if( length( gridList[[i]] ) < min_group_size ){

			cell_count_to_small = cell_count_to_small + 1

		} else {

			# If the number of points in the cell is less than the recursive threshold or not 
			#	in recursive mode perform closest calculation on cell
			if( length( gridList[[i]] ) < recusive.threshold || !recursiveMode ){

				# Calculate with cells need to be checked for closest 4
				cells_to_compare <- c( i , i - 1, i + 1, i - n.xGrids, i - n.xGrids + 1 , i - n.xGrids - 1, i + n.xGrids, i + n.xGrids + 1, i + n.xGrids - 1 )
			
				# remove cells from outside boundry and form list of cells to check
				points_to_check <- unlist(gridList[ cells_to_compare[cells_to_compare[] >= 0 & cells_to_compare[] <= n.xGrids * n.yGrids ] ])
			
				# Get the points to check for smallest distance and the points that need to be calculated.
				table_to_check <- table[points_to_check,] 
				table_points_to_do <- table[ unlist(gridList[i]), ]

				# Go through each point in the inner cell and calculate nearest 4 mean.
				for( j in 1:length( unlist( gridList[i] ) )  ){
					# Get x and y for point to calculate
					x.pos = table_points_to_do[j, x.col ]
					y.pos = table_points_to_do[j, y.col ]

					# Distance Calculation function
					distance <- function( x0 , y0 , x1 , y1 ){
						return (sqrt( abs( (x0 - x1)^2 +(y0 - y1)^2 ) ))
					}


					# Get a list of all points to compare against
					smallBox<- table_to_check[ ( distance( table_to_check[,x.col] , table_to_check[,y.col] , x.pos , y.pos ) != 0) ,]

					# Calculate the distance to each point to compare against
					smallBox[,"distance"] <-  distance( x.pos, y.pos, smallBox[,x.col] , smallBox[,y.col])
		
					# Order the distances to find the closest 4
					subSmall <- smallBox[ order(smallBox[,"distance"]), ][1:4,]

					# Reset row indexes
					rownames(subSmall) <- 1:nrow(subSmall)

					# Create a table of just 5 points to calculate the mean for
					subSmall[ nrow(subSmall) + 1, ] <- c( x.pos , y.pos, 0 )
	
					# Calculate the x and y means
					avg <- colMeans( subSmall )

					# Add mean value to output table
					closestOutput[ nrow(closestOutput) + 1, ] <- c( avg[ x.col ] , avg[ y.col ] )
				}

			} else {
				closestOutput <- rbind( closestOutput , meanPlotDS( table[ unlist(gridList[i]), x.col ], table[ unlist(gridList[i]), y.col ]  , ceiling(grid.dim/2)) )
			}

		}

		
	} 

	# Undo standardised scale
	closestOutput[,x.col] <- (closestOutput[,x.col] * sqrt(var(table.save[,x.col]))) + mean(table.save[,x.col])
	closestOutput[,y.col] <- (closestOutput[,y.col] * sqrt(var(table.save[,y.col]))) + mean(table.save[,y.col])

	# Return table of mean output values 
	return( closestOutput )

}

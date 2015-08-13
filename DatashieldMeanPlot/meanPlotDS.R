
meanPlotDS <- function( table=NULL , xCol=NULL , yCol=NULL , grid.dim=10 , recursiveMode=TRUE ) {
	# Save original table for later - Required for undoing standardised scale
	table.save = table

	# Group size
	min_group_size = 5

	# Recusive threshold 
	#	- If recursiveMode = TRUE this is the cell value at which a meanPlot will be recursivly called onto a cell
	recusive.threshold = 500

	# Initalised output dataframe
	colClasses = c( typeof(table[, xCol]) , typeof(table[, yCol])  )
	col.names = c( xCol, yCol )

	closestOutput <- read.table(text = "",
                 colClasses = colClasses,
                 col.names = col.names)

	# Remove unneeded columns from table
	table <- table[, c( xCol, yCol ) ]

	# Standardise scale
	table[, xCol ] <- (table[, xCol ] - mean(table[, xCol] ) )/sqrt(var(table[, xCol ]))
	table[, yCol ] <- (table[, yCol ] - mean(table[, yCol] ) )/sqrt(var(table[, yCol ]))

	# Get min values
	x.min <- min(table[, xCol ], na.rm = TRUE)
	y.min <- min(table[, yCol ], na.rm = TRUE)
	x.max <- max(table[, xCol ], na.rm = TRUE)
	y.max <- max(table[, yCol ], na.rm = TRUE)

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
		xGrid <- floor( ( table[i, xCol ] - x.min ) / gridWidth ) + 1
		yGrid <- floor( ( table[i, yCol ] - y.min ) / gridHeight ) + 1

		gridIndex = xGrid + ( yGrid * n.xGrids ) - n.xGrids

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
					x.pos = table_points_to_do[j, xCol ]
					y.pos = table_points_to_do[j, yCol ]

					# Distance Calculation function
					distance <- function( x0 , y0 , x1 , y1 ){
						return (sqrt( abs( (x0 - x1)^2 +(y0 - y1)^2 ) ))
					}


					# Get a list of all points to compare against
					smallBox<- table_to_check[ ( distance( table_to_check[,xCol] , table_to_check[,yCol] , x.pos , y.pos ) != 0) ,]

					# Calculate the distance to each point to compare against
					smallBox[,"distance"] <-  distance( x.pos, y.pos, smallBox[,xCol] , smallBox[,yCol])
		
					# Order the distances to find the closest 4
					subSmall <- smallBox[ order(smallBox[,"distance"]), ][1:4,]

					# Reset row indexes
					rownames(subSmall) <- 1:nrow(subSmall)

					# Create a table of just 5 points to calculate the mean for
					subSmall[ nrow(subSmall) + 1, ] <- c( x.pos , y.pos, 0 )
	
					# Calculate the x and y means
					avg <- colMeans( subSmall)

					# Add mean value to output table
					closestOutput[ nrow(closestOutput) + 1, ] <- c( avg[ xCol ] , avg[ yCol ] )
				}

			} else {
				closestOutput <- rbind( closestOutput , meanPlot( table[ unlist(gridList[i]), ] , xCol , yCol , ceiling(grid.dim/2)) )
			}

		}

		
	} 

	# Undo standardised scale
	closestOutput[,xCol] <- (closestOutput[,xCol] * sqrt(var(table.save[,xCol]))) + mean(table.save[,xCol])
	closestOutput[,yCol] <- (closestOutput[,yCol] * sqrt(var(table.save[,yCol]))) + mean(table.save[,yCol])

	# Return table of mean output values 
	return( closestOutput )



}



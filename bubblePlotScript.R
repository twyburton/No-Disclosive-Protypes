bubblePlot <- function( table , xCol, yCol, xGridSize, yGridSize , cellMin ){

	# Prepare inputs
	xGridSize <- floor(xGridSize)
	yGridSize <- floor(yGridSize)


	# Get max and min of axis
	xMax <- ceiling(max(table[, xCol ], na.rm = TRUE))
	xMin <- floor(min(table[, xCol ], na.rm = TRUE))
	yMax <- ceiling(max(table[, yCol ], na.rm = TRUE))
	yMin <- floor(min(table[, yCol ], na.rm = TRUE))

	# Create grid for plot
	plot_width <- ceiling(xMax - xMin)
	plot_height <- ceiling(yMax - yMin)
	
	xCells <- ceiling( plot_width / xGridSize + 1)
	yCells <- ceiling( plot_height / yGridSize  + 1)

	# Create array
	data_array <- array()
	
	# Go though each row of table and add 1 to cell where values fall in bounds 
	for ( i in 1: nrow(table) ) {
		x_pos = floor( ( table[ i , xCol ] - xMin ) / xGridSize )
		y_pos = floor( ( table[ i , yCol ] - yMin ) / yGridSize )

		#x_pos = floor( ( table[ i , xCol ] - xMin ) / xCells )
		#y_pos = floor( ( table[ i , yCol ] - yMin ) / yCells )


		array_index <- x_pos + (xCells * y_pos ) + 1
		#array_index <- x_pos + (plot_width * y_pos ) + 1
	
		if ( is.na( data_array[ array_index ] ) ) {
			data_array[array_index ] <- 1
		} else {
			data_array[array_index ] <- data_array[array_index ] + 1
		}	
	}

	# Create table to plot
	x_plot <- array()
	y_plot <- array()
	frequency_plot <- array()

	for ( i in 1:length( data_array ) ){
		x_plot[i] <- xMin + ( (i-1) %% xCells) * xGridSize + (xGridSize/2)
		y_plot[i] <- yMin + floor( (i-1) / xCells ) * yGridSize + (yGridSize/2)
		if( is.na(data_array[i] ) || data_array[i] < cellMin){
			frequency_plot[i] <- NA
		} else {
			frequency_plot[i] <- data_array[i]
		}

	}

	symbols(x_plot,y_plot,frequency_plot, xlab = xCol, ylab = yCol)
	grid()

}


raw_data <- read.csv("O:\\\\Documents\\Data\\ALSPAC\\ALSPAC.csv", header=TRUE )  # read csv file 
bubblePlot( raw_data , "BMI.7" , "wt.7", 2 , 2 , 5 )

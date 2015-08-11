## Is a the point inside the circle
inCircle <- function( x1 , y1 , xc , yc , r ) {
	return (( x1 - xc )^2 + ( y1 - yc )^2 < r^2)
}

closest5 <- function( table , xCol , yCol ) {
	original_table = table

	## Group size
	group_size = 5
	max_group_size = 20

	## Get columns from table that are being used
	table <- table[, c( xCol, yCol ) ]



	## Initalised output dataframe
	colClasses = c( typeof(table[, xCol]) , typeof(table[, yCol])  )
	col.names = c( xCol, yCol )

	closestOutput <- read.table(text = "",
                 colClasses = colClasses,
                 col.names = col.names)

	n_outer_loops = 0;

#########################
	while( nrow(table) > group_size ){
	
		print( paste( "Rows Remaining: " , nrow(table) ) )

		xMin <- min(table[, xCol ], na.rm = TRUE)
		yMin <- min(table[, yCol ], na.rm = TRUE)
		xMax <- max(table[, xCol ], na.rm = TRUE)
		yMax <- max(table[, yCol ], na.rm = TRUE)

		xDiff = xMax - xMin
		yDiff = yMax - yMin


		## Get row with lowest value of x
		start = table[ table[, xCol ] == xMin, ]
	
		## Get start circle center
		start_x = start[ 1, xCol]
		start_y = start[ 1, yCol]

		max_loops = 1000
		current_group_size = group_size

		n_in_circle = 0;
		n_rows = 0


		while ( n_rows != current_group_size ){

			n_loop = 0;
			check_radius = xDiff * 0.1

			while (  n_loop < max_loops && n_rows != current_group_size) {
		
				## Change radius 
				if( n_loop > 0 ){
					if( n_rows > group_size){
						check_radius = abs( check_radius - (3/n_loop))
					} else if ( n_rows < group_size) {
						check_radius = check_radius + (3/n_loop)
					} 
				} 
		

				n_in_circle = table[ inCircle(  table[,xCol]  ,  table[,yCol]  , start_x , start_y , check_radius ) , ]
				n_rows = nrow( n_in_circle )


				n_loop = n_loop + 1

			}

			if( n_loop == max_loops && n_rows != current_group_size ){
				current_group_size = current_group_size + 1
			
				if( current_group_size > max_group_size){
					break
				}
				print( paste( "Increasing Group Size: ", current_group_size ))
				print(check_radius)

			}

		}

		if( n_rows != current_group_size ){
			print("ERROR")
			break
		}


		## Get the average of selected 5
		avg = colMeans( n_in_circle )

		print(paste0("Group Size:",  n_rows, " Avg x:", avg[ xCol ], " Avg y:", avg[ yCol ], " Attemps: ", n_loop ))

		columns_to_remove = which( inCircle(  table[,xCol]  ,  table[,yCol]  , start_x , start_y , check_radius ) )
		table = table[ -columns_to_remove, ]

		closestOutput[ nrow(closestOutput) + 1, ] <- c( avg[ xCol ] , avg[ yCol ] )
		

		## DEBUG CURRENT PLOT


		plot( closestOutput[,xCol] , closestOutput[,yCol], xlab = xCol, ylab = yCol, col="black")
		points( n_in_circle[,xCol], n_in_circle[,yCol] , col="red")		
		

	}
#############


	plot( closestOutput[,xCol] , closestOutput[,yCol], xlab = xCol, ylab = yCol, col="green")


 ### POST PLOT
	#plot( original_table [,xCol] , original_table [,yCol], xlab = xCol, ylab = yCol, col="red")
	#points( closestOutput[,xCol] , closestOutput[,yCol], col="black")

	
}



## Testing

#data <- read.csv("O:/Documents/Data/ALSPAC/ALSPAC.csv", header=TRUE )  # read csv file 
#closest5( data , "BMI.7" , "wt.7" )


data <- read.csv("O:/Documents/Data/New_HOP_Data/HOP_simulated_data.csv", header=TRUE )  # read csv file 
system.time( closest5( data[1:10000,] , "LAB_HDL" , "LAB_TSC" ) , TRUE )
#points( data[,"LAB_HDL"] , data[,"LAB_TSC"], col="red")




#data = data.frame( c(10,17,11,12,16,14), c(15,18,23,45,50,32), c(13,22,64,34,10,13) )
#closest5( data , 1 , 2 )


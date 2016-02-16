#creando unos datitos de juguete
minimo <- runif(1)
maximo <- runif(1)*10
df1 <- as.data.frame(seq(minimo,maximo,by=0.1))
names(df1)=c("x1")

#----------------------------- NECESSARY PARAMETERS -----------------------------
var_o <- df1$x1   #variable we will use to make the overlapping subsets
n_int <- 10       #number of intervals we want
p <- 0.2          #proportion of each interval that should overlap with the next


#----------------------------- CREATING THE INTERVALS -----------------------------
#this section will create a data frame in which we will construct overlapping intervals
intervals_centers <- seq(min(var_o),max(var_o),length=n_int)  #basic partition = centers
interval_length <- intervals_centers[2]-intervals_centers[1]  #to create the overlaps of p% of this length
intervals <- data.frame(centers=intervals_centers)            #create a data frame
#create the overlapping intervals  
  intervals$min <- intervals_centers - (0.5+p)*interval_length                     
  intervals$max <- intervals_centers + (0.5+p)*interval_length
#decent name for the intervals e.g    [5.34;6.53)     [6.19;7.39)
intervals$name <- with(intervals, sprintf("[%.2f;%.2f)",min,max))

#function that will split the variable according to the invervals
#this function returns a list
res <- lapply(split(intervals,intervals$name), function(x){
  return(df1[var_o> x$min & var_o <= x$max,])
})

res



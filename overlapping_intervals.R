#CREATING A TOY DATASET
minimo <- runif(1)
maximo <- 1+runif(1)*9
df <- data.frame(x1=seq(minimo,maximo,by=0.1))
df$x2 <- c(rnorm(n=floor(dim(df)[1]/2),mean=10,sd=2),  #let's make this variable induce some clusters
           rnorm(n=ceiling(dim(df)[1]/2),mean=0,sd=1))
plot(df)

################### IMPORTANT
#BEFORE THIS SECTION IT IS IMPORTANT FOR THE DATA FRAME THAT WILL BE USED TO BE CALLED "df"
#ALSO I AM ASSUMING WE CAN TAKE "x1" AS A VARIABLE FROM "df" TO SUBSET
#THIS VARIABLE HAS TO BE NUMERIC AND NON CATEGORICAL
#I SUGGEST WORKING WITH THE TOY DATA SET FIRST AND THEN MOVING ON TO TRY OTHER ONES
####################

#----------------------------- NECESSARY PARAMETERS -----------------------------
var_o <- df$x1   #variable we will use to make the overlapping subsets
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
res <- lapply(split(intervals,intervals$name), function(x){   
  return(df[var_o> x$min & var_o <= x$max,])     #res will be a list with each element res[i]
})                                                #being the points on the i'th subset

#res

#----------------------------- NEXT STEPS -----------------------------
#ITERATE EVERY ELEMENT OF THE LIST (res[i]) AND CLUSTERIZE INSIDE
#SEE HOW THEY ARE LABELED
#DETERMINE WHICH CLUSTERS SHARE POINTS AND JOIN THEM (PUT A 1 ON THE ADJACENCY MATRIX BASICALLY)
#MAGIC



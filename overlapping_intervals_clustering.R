#CREATING A TOY DATASET
minimo <- runif(1)*1
maximo <- 1+runif(1)*9
df <- data.frame(x1=seq(minimo,maximo,by=0.001))
df$x2 <- c(rnorm(n=floor(dim(df)[1]/2),mean=10,sd=2),  #let's make this variable induce some clusters
           rnorm(n=ceiling(dim(df)[1]/2),mean=0,sd=1))
plot(df)

################### IMPORTANT
#BEFORE THIS SECTION IT IS IMPORTANT FOR THE DATA FRAME THAT WILL BE USED TO BE CALLED "df"
#ALSO I AM ASSUMING WE CAN TAKE "x1" AS A VARIABLE FROM "df" TO SUBSET
#THIS VARIABLE HAS TO BE NUMERIC AND NON CATEGORICAL
#I SUGGEST WORKING WITH THE TOY DATA SET FIRST AND THEN MOVING ON TO TRY OTHER ONES
####################

#----------------------------- PACKAGES -----------------------------

#install.packages("fpc")
#library(fpc)

#----------------------------- NECESSARY PARAMETERS -----------------------------
var_o <- df$x1   #variable we will use to make the overlapping subsets
n_int <- 5       #number of intervals we want
p <- 0.1          #proportion of each interval that should overlap with the next
#parameters for dbscan
eps <- 0.7            #epsilon makes the number of clusters VERY unstable  !!!!!
p_noise <- 0.05       #

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


#ITERATE EVERY ELEMENT OF THE LIST (res[i]) AND CLUSTERIZE INSIDE
ints<-list()
counter1<-1;counter2<-1

for(i in 1:(n_int-1)){
  df1<-as.data.frame(res[[i]])
  df2<-as.data.frame(res[[i+1]])
  
  if(i==1){
    MinPts <- p_noise*dim(df1)[1]
    result1<-(dbscan(df1,eps=eps,MinPts=MinPts,showplot = TRUE))
          }else{result1 <- result2
                }
  df1$cluster1 <- result1$cluster   #use the results for the last iteration
                                    #this ensures that the cluster labels will be correct for the adj. matrix
  
  MinPts <- p_noise*dim(df2)[1]
  result2<-(dbscan(df2,eps=eps,MinPts=MinPts,showplot = TRUE))
  df2$cluster2 <- result2$cluster
  
  intersection <- merge(df1,df2,all=TRUE)            #points in the intersection
  intersection[is.na(intersection)] <- 0
  ints[[i]]<-as.data.frame(unique(intersection[3:4]))               #list of all the clusters that intersect
  
        }

#----------------------------- GENERATE ADJACENCY MATRIX -----------------------------
adjacency_matrix <- data.frame()





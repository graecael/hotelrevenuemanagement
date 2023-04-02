library(dplyr)
library(fastDummies)
library(cluster)

set.seed(8)
data <- customerds[sample(nrow(customerds), size = 8400)]
data <- data[, c('Age1', 'AverageLeadTime', 'LodgingRevenue', 'OtherRevenue', 'RoomNights', 'PersonsNights', 'NumSR', 'DistributionChannel', 'CheckInDate')]

str(data)

dataset <- data %>% select_if(is.numeric)
factor <- data %>% select_if(is.factor)

# transform factor into numeric
factor <- dummy_cols(factor,
                     remove_most_frequent_dummy = FALSE)
factor <- factor[, !'DistributionChannel']

# finalise dataset
data <- cbind(dataset, factor, data$CheckInDate)
colnames(data)[12] <- 'CheckInDate'
# exclude date 
data1 <- cbind(dataset, factor) 


# scale 
customer.scale <- scale(data1)
# distance 
customer.dist <- dist(customer.scale)

# elbow method
fviz_nbclust(customer.scale, kmeans, method = 'wss') + labs(subtitle = 'Elbow Method')

# avg silhouette method
fviz_nbclust(customer.scale, kmeans, method = 'silhouette') + labs(subtitle = 'Silhouette Method')

# gap statistics
set.seed(8)
gap_stat <- clusGap(customer.scale, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
print(gap_stat, method = 'firstmax')
fviz_gap_stat(gap_stat)


# k-means clustering k=5
set.seed(8)
km.out <- kmeans(customer.scale, centers = 5, iter.max = 100)
# export centers to excel for interpretation
km.out$centers
write.csv(km.out$centers, file = '/Users/phyllisyen/Desktop/BC2407 ANALYTICS 2/Project/clusters.csv')
# add cluster center into the dataset
data <- cbind(data, km.out$cluster)
colnames(data)[13] <- "Cluster"

# visualise
km.cluster <- km.out$cluster
fviz_cluster(list(data = customer.scale, cluster = km.cluster))


# CheckInDate and Cluster
ggplot(data, aes(y = factor(month(CheckInDate)), x = Cluster, color = Cluster)) + geom_jitter() + labs(title = 'Number of Organisers from each Cluster throughout the year', y = 'Month')

# NumSR and Cluster
ggplot(data, aes(x = NumSR, fill = factor(Cluster))) + geom_bar(position="fill") + labs(title = 'Proportion of SR made by each Cluster', x= 'Number of SR', y='Percentage')


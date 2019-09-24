library(RCurl)
library(Matrix)
library(tibble)

u.user <- (read.csv(text=(getURL('http://cours.polymtl.ca/MDesmarais/log6308/Tp/20193/u.user.csv', userpwd='20113:20113')), sep='|', header=T))
u.item <- (read.csv(text=(getURL('http://cours.polymtl.ca/MDesmarais/log6308/Tp/20193/u.item.csv', userpwd='20113:20113')), sep='|', header=T))
u.data <- (read.csv(text=(getURL('http://cours.polymtl.ca/MDesmarais/log6308/Tp/20193/u.data.csv', userpwd='20113:20113')), sep='|', header=T))

m <- sparseMatrix(u.data[,1],u.data[,2],x=u.data[,3])
rownames(m) <- paste('u', 1:nrow(m), sep='')
colnames(m) <- paste('i', 1:ncol(m), sep='')

m <- as.matrix(m)
m[m==0] <- NA

###########################################################
##Question 1 : Average rating per job and age

# Step 1 : We create a table : userId, Job, Age, Rating
user_data_merge <- merge(u.user, u.data, by.x='id', by.y='user.id')
user_data_merge <- subset(user_data_merge, select = -c(gender, zip, item.id, timestamp) )
# Average rating per Job
job_rating_average <- aggregate(user_data_merge[, 4], list(user_data_merge$job), mean)
# Average rating per Age
age_rating_average <- aggregate(user_data_merge[, 4], list(user_data_merge$age), mean)

###########################################################
##Question 2 : 10 movie the most similar to "Star Trek V: The Final Frontier (1989)"
##             using the method of cosinus and the method of correlation
#Step 1 : extract "Star Trek V: The Final Frontier (1989)" from u.item data frame
star_trek_1989 <- u.item[u.item[, "movie.title"] == "Star Trek V: The Final Frontier (1989)",]
#star streck itemId is 450
m_t = m
m_t[is.na(m_t)] <- 0
w <- cor(m_t)
w_list <- w[order(w[,450], decreasing = TRUE),]
w_list <- subset(w_list, select = c(i450) )
w_list <- head(w_list, 11)
w_list <- tail(w_list, 10)
#result_1 =  cor(m) 


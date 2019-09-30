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

###########################################################
##Question 2 : Approche Item Item
## Cosinus entre un vecteur v et chaque colonne dela matrice m


m.sparse <- sparseMatrix(u.data[,1],u.data[,2],x=u.data[,3])
rownames(m.sparse) <- paste('u', 1:nrow(m.sparse), sep='')
colnames(m.sparse) <- paste('i', 1:ncol(m.sparse), sep='')

cosinus.vm <- function(v,m) { n <- sqrt(colSums(m^2)); (v %*% m)/(n * sqrt(sum(v^2))) }

# Trouve les indexes des premières 'n' valeurs maximales d'une matrice
max.nindex <- function(m, n=5) {
  i <- order(m, decreasing=TRUE)
  return(i[1:n])
}
min.nindex <- function(m, n=5) {
  i <- order(m)
  return(i[1:n])
}

sum(m.sparse[,450]>0)

u.item$movie.title[450]

m <- as.matrix(m.sparse)
m[m==0] <- NA
sum(!is.na(m.sparse[,450]))

n.voisins <- 20 + 1
votes.communs <- (colSums((m.sparse[,450] * m.sparse) > 0)) # nombre de votes communs
## Histogramme du nombre de films communs avec Star Trek V

hist(round(m.sparse[,450]>0) %*%  round(as.matrix(m.sparse)>0))

## visualisation des distances avec les autres films
distance.na.450 <- sqrt(colSums((m[,450] - m)^2, na.rm=T)) # ignore les valeurs manquantes
distance.450 <- sqrt(colSums((m.sparse[,450] - m.sparse)^2)) # valeurs manquantes à 0
distance.450[450]            # petite vérification

par(mfrow=c(2,2))
hist(distance.450)                      # histogramme des distances
hist(distance.na.450)                   # idem
distance.dist.na.450 <- as.matrix(dist(t(m)))[450,] # utilisation de la fonction dist avec NA
distance.dist.450 <- as.matrix(dist(t(m.sparse)))[450,] # fonction dist avec val. manq. = 0
hist(distance.dist.na.450)              # nouvel histogramme pour dist
hist(distance.dist.450)                 # idem

## calcul Des voisins
(i.distance.450 <- min.nindex(distance.450, n.voisins))

votes.communs[i.distance.450]           # bonne nouvelle : pas de voisins sans votes communs, mais tout de même plusieurs voisins qui n'ont qu'un vote en commun.


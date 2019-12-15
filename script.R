#Used packages
library(tidyverse)
library(caret)
library(randomForest)
library(fastDummies)
library(treemap)


#Importing data
wildlife <- read.csv("wildlife.csv",stringsAsFactors = FALSE)
iucn <- read.csv("species.csv",stringsAsFactors = FALSE)
iucn <- iucn %>% separate(Scientific.Name,into = c("Genus","sec"),sep=" ")
#Joining Data
manupilation_df <- merge(x=wildlife,y=iucn,by="Genus",all.x = TRUE)
manupilation_df <-na.omit(manupilation_df)

#Data Manupilation 
manupilation_df <- manupilation_df[,c(3,11,12,13,29)]
manupilation_df$Conservation.Status[manupilation_df$Conservation.Status == ""] <- "Safe"
manupilation_df <- manupilation_df %>% filter(Conservation.Status %in% c("Endangered","Safe","Species of Concern","Threatened"))
manupilation_df <- manupilation_df %>% mutate(Reported.Quantity = Exporter.reported.quantity + Importer.reported.quantity)
manupilation_df <- manupilation_df[,-c(2,3)]
work_df <- manupilation_df


#EDA
ggplot(work_df,aes(x=1,fill=App.))+geom_bar(color="white")+coord_polar(theta="y") + theme_void()
ggplot(work_df,aes(x=Conservation.Status))+geom_histogram(stat="count")+coord_flip()
ggplot(work_df,aes(x=App. , fill = Conservation.Status))+geom_bar()
table(work_df$Conservation.Status,work_df$Term)

t1 <- filter(wildlife,!is.na(Term))
t1p <- group_by(t1, Term )
t2p <- dplyr::summarise(t1p,  count=n())
t2p <- filter(t2p, count>300)
treemap(t2p, index="Term", vSize="count", type="index", 
        palette="Pastel2", title="WildLife Trade Term Treemap", fontsize.title=12)

#Kmeans
clustering_df <- work_df[,-3]
clustering_df <- clustering_df %>% mutate(App.=(str_length(App.)-1)/2)
clustering_df$Term <- as.factor(clustering_df$Term)
clustering_df <- dummy_columns(clustering_df)
clustering_df <- clustering_df[,-2]


wss <- 0
for (i in 1:6 ) { 
  wss[i] <- sum(kmeans(clustering_df, centers = i)$withinss) 
}  

plot(1:6, wss, type = "b", xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares")

km_out <- kmeans(clustering_df,centers=3,nstart = 20)
hist(km_out$cluster)
table(km_out$cluster)



#Prepare for model 
work_df <- cbind(work_df,km_out$cluster)
names(work_df)[names(work_df) == "km_out$cluster"] <- "cluster"
work_df$cluster <- as.factor(work_df$cluster)
#split
n_obs <- nrow(work_df)
permuted_rows <- sample(n_obs)
work_df_shuffled <- work_df[permuted_rows,]
index <- round(n_obs * 0.75)
index <- as.integer(index)
train_data <- work_df_shuffled[1:index,]
test_data <- work_df_shuffled[(index+1):nrow(work_df_shuffled),]


#train
control <- trainControl(method="repeatedcv", number=10, repeats=3)
set.seed(7)
fit.rf <- train(Conservation.Status ~ App. + Reported.Quantity + cluster,
                data= train_data, method="rf", trControl=control)
#test
rf_predictions <- predict(fit.rf,test_data)
test_data$Conservation.Status <- as.factor(test_data$Conservation.Status)
m <- confusionMatrix(rf_predictions,test_data$Conservation.Status)







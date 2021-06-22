
library(rio)         # dloading data

library(dplyr)       # data manipulation

library(ggplot2)     # visualization

library(factoextra)  # visualization

files <- dir("data", full = T)


if(length(files)==0 ){
   warning("FIRST YOU MUST CREATE A NEW R PROJEC THEN COPY THE FILES DOWNLOADED FROM GIT HUB 
           AND PASTE ALL INTO THE NEW PROJECT FOLDER") }



stores <- import(files[1], which = 1)%>%janitor::clean_names()


paste0("There are ",sum(duplicated(stores)), " duplicated observations") 


visdat::vis_miss(stores)


skimr::skim(stores)


#============================================================================================


ggplot(stores, aes(marketing_spend, revenue))+ 
geom_point(size = 4, alpha= 0.6)+
theme_minimal()  +
scale_x_continuous(labels = scales::dollar) +
scale_y_continuous(labels = scales::dollar) +
ggtitle("Marketing Spend vs Revenue")+
theme(
axis.text.x=element_text( color="black"),
plot.title = element_text(size = 17),
strip.text.x = element_text(size = 14))

#============================================================================================



# Lets check if the algorithm kmeans can indentify these clusters 
k_means_data <- stores %>%
   select(revenue, marketing_spend)


set.seed(111)
# Set k equal to the optimal number of clusters
num_clusters <- 2

# Run the k-means algorithm 
k_means <- kmeans(k_means_data, num_clusters, iter.max = 15, nstart = 25)


stores <- stores %>%
   mutate( k_means_cluster = k_means$cluster)  



ggplot(stores, aes(marketing_spend, revenue, color = as.factor(k_means_cluster)))+ 
   geom_point(size = 4, alpha= 0.6) +
   geom_smooth(method = "lm",se = F)+
   theme_minimal()+
   scale_x_continuous(labels = scales::dollar) +
   scale_y_continuous(labels = scales::dollar) +
   scale_color_manual(name="Clusters",values=c("#EC2C73","#29AEC7")) +
   ggtitle('Clusters by Marketing Spend vs Revenue')+
   ylab("Revenue") +
   xlab("Marketing Spend") 
   
#============================================================================================
pop <- read.csv(files[2])%>%
janitor::clean_names()%>%
select(city,state,x2015_estimate)

paste0("There are ", stores%>% nrow, " rows in the datase before the join")

company_pop <- stores %>% 
inner_join(pop, by = c("city", "state"))

paste0("There are ", company_pop %>% nrow , " rows in the datase after the join")


k_means_data <- company_pop %>%
select(revenue, marketing_spend,x2015_estimate)


fviz_nbclust(k_means_data, kmeans, method = "wss")


fviz_nbclust(k_means_data, kmeans,method = "silhouette")

#============================================================================================

set.seed(111)

# Set k equal to the optimal number of clusters
num_clusters <- 3

# Run the k-means algorithm 
k_means <- kmeans(k_means_data, num_clusters, iter.max = 5, nstart = 25)


company_pop <- company_pop %>%
mutate( k_means_cluster = k_means$cluster)  


ggplot(company_pop, aes(marketing_spend, revenue, color = as.factor(k_means_cluster) ))+ 
geom_point(size = 4, alpha= 0.6) +
geom_smooth(method = "lm",se = F)+
theme_minimal()  +
scale_x_continuous(labels = scales::dollar) +
scale_y_continuous(labels = scales::dollar) +
scale_color_manual(name="Clusters",values=c("#EC2C73","#29AEC7", 
"#FFDD30")) +
ggtitle('Clusters by Marketing Spend vs Revenue')+
ylab("Revenue") +
xlab("Marketing Spend") 

#============================================================================================

company_pop <- company_pop %>%
mutate(k_means_cluster = as.factor(
ifelse(revenue >27000 & k_means_cluster == 2, 1,k_means_cluster )))


company_pop %>%
group_by(k_means_cluster)%>%
summarise(pop_mean = mean(x2015_estimate))


lm( revenue ~ marketing_spend, company_pop %>% filter(k_means_cluster == 1))

lm( revenue ~ marketing_spend, company_pop %>% filter(k_means_cluster == 2))

lm( revenue ~ marketing_spend, company_pop %>% filter(k_means_cluster == 3))


#============================================================================================

company_pop %>%
filter(k_means_cluster == 1 & new_expansion == "New")%>%
mutate(`Return per dollar invested` = round(revenue /marketing_spend,2))%>%
select(city,`Return per dollar invested`)%>%
ggplot(aes(city,`Return per dollar invested`))+
scale_y_continuous(labels = scales::dollar) +
geom_col() +
theme_minimal()+
ggtitle('Best Stores to invest in Marketing')+
xlab("City")  



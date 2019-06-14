team3_forest<-'https://raw.githubusercontent.com/MongMong11/Biological-Resource-Sampling-Survey-Technology/master/Cat_Wu-Shi-Keng_artificial_forest.csv'
data <- data.table::fread(team3_forest, encoding = 'UTF-8')
data<-data.frame(data)
data

data<-as.data.table(data)
data

# cauculate specie's number - step 1

left = function(text, num_char) {
  substr(text, 1, num_char)
}

individual<-left(data[,2],2)
DataTreeID<-unique(paste(individual, data[,3]))

DataTreeID


DBH_S<-data[,.(DBH.1 = DBH <= 3 ),by=species]
DBH_S<-as.data.table(DBH_S)
DBH_S<-DBH_S[DBH.1 %in% c("TRUE")]
as.data.frame(DBH_S)
DBH_S<-table(DBH_S)
View(DBH_S)

DBH_L<-data[,.(DBH.3= DBH>10),by=species]
DBH_L<-as.data.table(DBH_L)
DBH_L<-DBH_L[DBH.3 %in% c("TRUE")]
as.data.frame(DBH_L)
DBH_L<-table(DBH_L)
View(DBH_L)

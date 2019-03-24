#download data
team3_forest<-'https://raw.githubusercontent.com/MongMong11/Biological-resource-sampling-survey-technology/master/0311_team3-forest%20.csv'
data <- data.table::fread(team3_forest, encoding = 'UTF-8')
data<-data.frame(data)
data

# cauculate specie's number - step 1

left = function(text, num_char) {
  substr(text, 1, num_char)
}

individual<-left(data[,2],2)
DataTreeID<-unique(paste(individual, data[,3]))

DataTreeID

# cauculate specie's number - step 2

name<-unique(data[,3])

#loop
i=1
c=c()
while (i<19) {
  a<-as.numeric(length(grep(name[i],DataTreeID)))
  c<-append(c,a)
  print(c)
  i=i+1
}

#result
count<-c
Den<-count/4
RD<-Den/sum(Den)*100

# Dominance

Cover_Area<-((data[,4])/2)^2*3.14/100
cbind(data, Cover_Area)
data<-as.data.table(data)
Do<-data[,.(Cover_Area.sum = sum(Cover_Area)),by=Species]
RDo<-Do/sum(Do)*100

# frequency

count2<-c(4,2,3,1,2,1,1,1,2,2,1,1)
Fr<-count2/4
RF<-Fr/sum(Fr)*100

# ivi
ivi<-RF+RDo+RD
sum(ivi)

# make data.frame

ivi_data<-cbind(name, count, Den, RD, Fr, RF, Do, RDo, ivi)
ivi_data<-as.data.frame(ivi_data)
View(ivi_data)

#output
write.csv(ivi_data, 'C:/Users/user/Desktop/ivi_data.csv', row.names = FALSE)

# calculate DBH class number
data<-as.data.table(data)

DBH_S<-data[,.(DBH.1 = DBH <= 3 ),by=Species]
DBH_S<-as.data.table(DBH_S)
DBH_S<-DBH_S[DBH.1 %in% c("TRUE")]
as.data.frame(DBH_S)
DBH_S<-table(DBH_S)
DBH_S

DBH_L<-data[,.(DBH.3= DBH>10),by=Species]
DBH_L<-as.data.table(DBH_L)
DBH_L<-DBH_L[DBH.3 %in% c("TRUE")]


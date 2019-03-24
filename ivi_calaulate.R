#download data
team3_forest<-'https://raw.githubusercontent.com/MongMong11/Biological-resource-sampling-survey-technology/master/0311_team3-forest%20.csv'
data <- data.table::fread(team3_forest, encoding = 'UTF-8')
data<-data.frame(data)
data

# cauculate specie's number - step 2

left = function(text, num_char) {
  substr(text, 1, num_char)
}

individual<-left(data[,2],2)
DataTreeID<-unique(paste(individual, data[,3]))

DataTreeID

# cauculate specie's number - step 2

name<-unique(data[,3])

i=1
while (i<13) {
  print(as.numeric(length(grep(name[i],DataTreeID))))
  i=i+1
}

# result

count<-as.numeric(c(16,1,2,1,2,1,1,1,1,1,1,1))
Den<-count/4
RD<-Den/sum(Den)*100

# Dominance
Do<-((data[,4])/2)^2*3.14/100
Do<-c(sum(Do[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,25)]),
sum(Do[c(20)]),
sum(Do[c(21,32)]),
sum(Do[c(22)]),
sum(Do[c(23,26)]),
sum(Do[c(24)]),
sum(Do[c(27)]),
sum(Do[c(28)]),
sum(Do[29]),
sum(Do[30]),
sum(Do[31]),
sum(Do[33]))
RDo<-Do/sum(Do)*100

# frequency

count2<-c(4,2,3,1,2,1,1,1,2,2,1,1)
Fr<-count2/4
RF<-Fr/sum(Fr)*100

# ivi
ivi<-RF+RDo+RD
sum(ivi)
# output

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


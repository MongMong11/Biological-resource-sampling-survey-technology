team3_forest<-'https://raw.githubusercontent.com/MongMong11/Biological-Resource-Sampling-Survey-Technology/master/Cat_Wu-Shi-Keng_artificial_forest.csv'
data <- data.table::fread(team3_forest, encoding = 'UTF-8')
data<-data.frame(data)
data

DBH<-data[,4]
Basal_Area<-0.005454 * DBH^2
species<-data[,3]
data<-cbind(species, Basal_Area)
View(data)

# cauculate specie's number - step 1

left = function(text, num_char) {
  substr(text, 1, num_char)
}

individual<-left(data[,2],2)
DataTreeID<-unique(paste(individual, data[,3]))

DataTreeID

# cauculate specie's number - step 2

Species<-unique(data[,3])
length(Species)

#loop

i=1
c=c()
number<-as.integer(length(Species))
while (i<number) {
  a<-as.numeric(length(grep(Species[i],DataTreeID)))
  c<-append(c,a)
  print(c)
  i=i+1
}


count<-c
Den<-count/4
RD<-Den/sum(Den)*100
Den_result_data<-cbind(Species, Den, RD)
Den_result_data<-as.data.frame(Den_result_data)
Den_result_data

Cover_Area<-((data[,4])/2)^2*3.14/100
data<-cbind(data, Cover_Area)
Do<-aggregate(DBH ~ species, data, sum)
colnames(Do) <- c("Species", "Do")
RDo<-Do[,2]/sum(Do[,2])*100
Do_result_data<-cbind(Do, RDo)
Do_result_data


unfinish_result<-merge(Den_result_data, Do_result_data)
View(unfinish_result)

team3_forest_Fr<-'https://raw.githubusercontent.com/MongMong11/Biological-Resource-Sampling-Survey-Technology/master/Cat_WuShiKeng_artificial_Forest_Fr.csv'
data2 <- data.table::fread(team3_forest_Fr, encoding = 'UTF-8')
data2<-data.frame(data2)
data2


count2<-data2[,2]
Fr<-count2/4
RF<-Fr/sum(Fr)*100
Species<-data2[,1]
Fr_result_data<-cbind(Species, Fr ,RF)
Fr_result_data<-as.data.frame(Fr_result_data)
View(Fr_result_data)

unfinish_result2<-merge(unfinish_result,Fr_result_data)
                        
View(unfinish_result2)

ivi<-RF+RDo+RD
sum(ivi)

ivi_data<-cbind(unfinish_result2, ivi)
ivi_data<-as.data.frame(ivi_data)
View(ivi_data)

write.csv(ivi_data, 'C:/Users/user/Desktop/ivi_data.csv', row.names = FALSE)

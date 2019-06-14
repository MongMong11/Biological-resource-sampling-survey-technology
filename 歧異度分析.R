team3_forest<-'https://raw.githubusercontent.com/MongMong11/Biological-Resource-Sampling-Survey-Technology/master/Cat_Wu-Shi-Keng_artificial_forest.csv'
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

species<-unique(data[,3])
length(species)

#loop

i=1
c=c()
number<-as.integer(length(species))+1
while (i<number) {
  a<-as.numeric(length(grep(species[i],DataTreeID)))
  c<-append(c,a)
  print(c)
  i=i+1
}
c<-as.numeric(c)
data<-as.data.frame(cbind(species, c))
View(data)
#p_i
p_i<-c/sum(c)

#Simpson
S<- sum((p_i)^2)
S

#Shannon
H<- -sum(p_i * log(p_i))
H

#N1
N1<-exp(H)
N1

#N2
N2<-1/S
N2

#E5
E5<-(N2-1)/(N1-1)
E5

data<-cbind(number-1,S,H,N1,N2,E5)
View(data)
write.csv(data, 'C:/Users/user/Desktop/diversity_data.csv', row.names = FALSE)

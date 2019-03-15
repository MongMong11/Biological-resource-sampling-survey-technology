m_forest <- read.table(header = TRUE, text = "
Sample_Area Species_Number IM
100 19 0
200 22 0
300 32 0
361.0110 33.1555 1
400 35 0
500 44 0
600 45 0
700 46 0
800 48 0
900 48 0
1000 50 0
1100 54 0
1200 54 0
1300 54 0
1400 55 0
")
attach(m_forest)
names(m_forest)

formation<-function(x){
  return(2.670238*x^0.42776)
}

formation2<-function(x){
  return(0.03928571*x+18.97293)  
}

library(ggplot2)
ggplot(data = m_forest) + 
  geom_point(mapping = aes(x = Sample_Area, y = Species_Number, colour = IM)) +
  stat_function(fun = formation, colour="MediumSpringGreen") +
  stat_function(fun = formation2, colour="Tomato") +
  theme(legend.position = "none")



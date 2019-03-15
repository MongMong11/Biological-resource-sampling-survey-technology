m_grass <- read.table(header = TRUE, text = "
Sample_Area Species_Number IM
25 16 0
50 19 0
75 20 0
85.4665 23.27645 1
100 24 0
125 24 0
150 30 0
175 31 0
200 33 0
225 36 0
275 36 0
300 36 0
325 39 0
375 40 0
400 40 0
")
attach(m_grass)
names(m_grass)

formation<-function(x){
  return(4.545692*x^0.36718)
}

formation2<-function(x){
  return(0.1*x+14.7298) 
  }

library(ggplot2)
ggplot(data = m_grass) + 
  geom_point(mapping = aes(x = Sample_Area, y = Species_Number, colour = IM)) +
  stat_function(fun = formation, colour="GreenYellow") +
  stat_function(fun = formation2, colour="Tomato") +
  theme(legend.position = "none")


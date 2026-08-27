###################################
# Fecha: Septiembre 2020          #
# Topico: Graficos                #
###################################

library(ggplot2)
library(grid)
library(gridExtra)

# Grafico informacion
bar <- data.frame(dose=c("Larger","Smaller","About the same"),
                  per=c(43,37,20)) 

g4=ggplot(data=bar,aes(x=dose,y=per)) +
  geom_bar(stat="identity",fill="steelblue") + 
  geom_text(aes(label=per), vjust=1.6, color="white", size=5.5)+
  scale_y_continuous(limits=c(0,100),breaks = seq(0,100,by=10)) +
  xlab("") +
  ylab("Percentage") + 
  ggtitle("Perceptions of poverty's evolution")+
  theme_linedraw()

g4
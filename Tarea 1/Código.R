
# Borrar lo que esté abierto
rm(list = ls())

#install.packages("ggbreak")
#install.packages('extrafont')
# Paquetes.
library(ggplot2)
library(tibble)
library(gridExtra)
library(dplyr)
library(Lock5Data)
library(ggthemes)
library(fun)
library(zoo)
library(corrplot)
library(maps)
library(mapproj)
library(scales)
library(ggbreak)
library(extrafont)
library(gtable)
library(gridExtra)
library(grid)
library(lattice)

#Para cargar las fuentes de letras. Lleva un tiempo.
font_import()
y
loadfonts(device = "win")

# Directorio
setwd("D:/Documentos/Maestría/UdeSA/4. Segunda parte/2. Herramientas computacionales/4. Data visualization/Tareas/Tarea 1")

# Cargamos las bases
df <- read.csv("data/gapminder-data.csv")
df2 <- read.csv("data/LoanStats.csv")

#Gráficos a replicar.####

##Gráfico 1.####

# Original
g1 <- ggplot(df, aes(x = gdp_per_capita, y = Electricity_consumption_per_capita)) + geom_point() 
g1 + facet_wrap(~Country)

# Modificado
g2 <- ggplot(df, aes(x = gdp_per_capita, y = Electricity_consumption_per_capita)) +
  geom_point(colour = "black") +
  facet_wrap(~ Country) +
  theme(strip.text = element_text(colour = 'black', face = "bold")) +
  theme(
    axis.title.y = element_text(angle = 0,
                                vjust = 1.1,
                                margin = margin(t = 0,
                                                r = -20,
                                                b = 0,
                                                l = 0))) +
  ylab("Consumo") +
  scale_y_continuous(labels = unit_format(unit = "",
                                          accuracy = 1,
                                          scale = 1e-3)) +
  scale_x_continuous(labels = unit_format(unit = "",
                                          scale = 1e-3)) +
  xlab("PBI") +
  theme(text = element_text(family = "Lucida Fax"))

grid.arrange(g2,
             top = textGrob("Consumo eléctrico y PBI\n(En miles. Valores per capita.)",
                            gp = gpar(fontfamily = "Lucida Fax",
                                      fontsize = 12),
                            x = 0,
                            hjust = 0
             ))


##Gráfico 2.####
#Otra forma de hacer los gráficos cuando hay que poner varios.
###Original.####
df2s <- subset(df2,grade %in% c("A","B","C","D","E","F","G"))
pb1<-ggplot(df2s,aes(x=loan_amnt)) +
  geom_histogram(bins=10,fill="cadetblue4") +
  facet_wrap(~grade, scale="free_y")
pb1

###Modificado.####
#1. Definir una función que haga gráficos, por ejemplo, histogramas.
ph <- function(df,mytxt) {
  ggplot(df,aes_string(x=mytxt)) +
  theme_bw() +
  geom_histogram(aes(y = stat(count) / sum(count)), bins = 10, fill = "cadetblue") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(labels = unit_format(unit = "",
                                            accuracy = 1,
                                            scale = 1e-4)) +
  theme(text = element_text(family = "Lucida Fax")) +
  ylab("") +
  theme(plot.title = element_text(hjust = 0.5, size = 10))
}

#2. Hacer un loop para armar varios gráficos con subsets de la base.
graf <- list()
s <- list()
t <- list()

for (i in c("A","B","C","D","E","F","G")) {
  s[[i]] <- subset(df2,grade == i)
  graf[[i]] <- ph(s[[i]],"loan_amnt") + ggtitle(i)
}

#3. Referencias
ref <- grid.arrange(
  grobTree(rectGrob(gp = gpar(alpha = 0)), textGrob("A - Mej. cal.", gp = gpar(fontfamily = "Lucida Fax", fontsize = 10))),
  grobTree(rectGrob(gp = gpar(alpha = 0)), textGrob("B", gp = gpar(fontfamily = "Lucida Fax", fontsize = 10))),
  grobTree(rectGrob(gp = gpar(alpha = 0)), textGrob("C", gp = gpar(fontfamily = "Lucida Fax", fontsize = 10))),
  grobTree(rectGrob(gp = gpar(alpha = 0)), textGrob("D", gp = gpar(fontfamily = "Lucida Fax", fontsize = 10))),
  grobTree(rectGrob(gp = gpar(alpha = 0)), textGrob("E", gp = gpar(fontfamily = "Lucida Fax", fontsize = 10))),
  grobTree(rectGrob(gp = gpar(alpha = 0)), textGrob("F", gp = gpar(fontfamily = "Lucida Fax", fontsize = 10))),
  grobTree(rectGrob(gp = gpar(alpha = 0)), textGrob("G - Peor cal.", gp = gpar(fontfamily = "Lucida Fax", fontsize = 10))),
  layout_matrix = rbind(c(1),
                        c(2),
                        c(3),
                        c(4),
                        c(5),
                        c(6),
                        c(7)
                        )
)

#4. Poner los gráficos en forma de grilla.
grid.arrange(
  graf[["A"]] + xlab(""),
  graf[["B"]] + xlab(""),
  graf[["C"]] + xlab(""),
  graf[["D"]] + xlab("Monto") + theme(axis.title.x = element_text(size = 10)),
  graf[["E"]] + xlab("Monto") + theme(axis.title.x = element_text(size = 10)),
  graf[["F"]] + xlab("Monto") + theme(axis.title.x = element_text(size = 10)),
  graf[["G"]] + xlab("Monto") + theme(axis.title.x = element_text(size = 10)),
  ref,
  nrow = 2,
  top = textGrob("Frecuencia relativa de préstamos por calificación\ncrediticia",
    gp = gpar(fontfamily = "Lucida Fax", fontsize = 12),
    x = 0,
    hjust = 0
  ),
  bottom = textGrob("Montos en 10 miles.",
                    gp = gpar(fontfamily = "Lucida Fax", fontsize = 11),
                    x = 0.5,
                    hjust = 0.5
                    )
)

##Gráfico 3.####
data <- subset(df2, grade == "A")

###Original.####
z <- ggplot(data, aes(funded_amnt,total_pymnt_inv,color=grade)) + geom_point() + stat_smooth(method=lm,color=2)
z

###Modificado.####
z2 <- ggplot(data, aes(funded_amnt,total_pymnt_inv,color=grade)) +
  geom_point() +
  stat_smooth(method = lm, color = "black") +
  labs(title = "Relation between principal payments\nand payments from investors",
       subtitle = "Is grade A safer for investors?") +
  xlab("Amount of debt to be paid") +
  ylab("Payments recieved\nfrom investor funding") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text())

z2 <- z2 + theme(plot.title = element_text(hjust = 0.5))
z2 <- z2 + theme(plot.subtitle = element_text(hjust = 0.5))
z2

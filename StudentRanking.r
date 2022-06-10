# libraries
library(tidyverse) 
library(cluster) 
library(dendextend) 
library(factoextra)
library(fpc) 
library(gridExtra)
library(readxl)

#load data, view, move ID to rows
alunos <- read.table("data/alunos.csv", sep = ";", header = T, dec = ",")
View(alunos)
rownames(alunos) <- alunos[,1]
alunos <- alunos[,-1]
View(alunos)

#         math    portuguese
#
# Dri     9       7
# Li      5       4
# Bru     6       6
# Mi      10      8
# Re      4       4
# ZZ      4       9

#method
d <- dist(alunos, method = "euclidean")
d

#         Dri         Li          Bru         Mi          Re
# Li      5.000000
# Bru     3.162278    2.236068
# Mi      1.414214    6.403124    4.472136
# Re      5.830952    1.000000    2.828427    7.211103
# ZZ      5.385165    5.099020    3.605551    6.082763    5.000000

#testing various methods
hc1 <- hclust(d, method = "single" )
hc2 <- hclust(d, method = "complete" )
hc3 <- hclust(d, method = "average" )
hc4 <- hclust(d, method = "ward.D" )

#dendrogram
plot(hc1, cex = 0.6, hang = -1)
plot(hc2, cex = 0.6, hang = -1)
plot(hc3, cex = 0.6, hang = -1)
plot(hc4, cex = 0.6, hang = -1)

#analyzing two dendrograms
rect.hclust(hc4, k = 2)

#comparing the average method with ward
dend3 <- as.dendrogram(hc3)
dend4 <- as.dendrogram(hc4)
dend_list <- dendlist(dend3, dend4)

#TANGLE, the smaller, the more equal the dendrograms are
tanglegram(dend3, dend4, main = paste("Emaranhado =", round(entanglement(dend_list),2)))

#now comparing the single method with complete
dend1 <- as.dendrogram(hc1)
dend2 <- as.dendrogram(hc2)
dend_list2 <- dendlist(dend1, dend2)

#TANGLE, the smaller, the more equal the dendrograms are
tanglegram(dend1, dend2, main = paste("Emaranhado =", round(entanglement(dend_list2),2)))

#creating 2 groups of students
grupo_alunos2 <- cutree(hc4, k = 2)
table(grupo_alunos2)

# grupo_alunos2
# 1 2
# 2 4

#transforming cluster output into data frame
alunos_grupos <- data.frame(grupo_alunos2)

#joining with the original base
Base_alunos_fim <- cbind(alunos, alunos_grupos)
View(Base_alunos_fim)

#         math    portuguese  grupo_alunos2   
#
# Dri     9       7           1         
# Li      5       4           2
# Bru     6       6           2
# Mi      10      8           1
# Re      4       4           2
# ZZ      4       9           2


#descriptive analysis
#averages of the variables per group
mediagrupo_alunos <- Base_alunos_fim %>% 
  group_by(grupo_alunos2) %>% 
  summarise(n = n(),
            Portugues = mean(Portugues), 
            Matematica = mean(Matematica))
df <- data.frame(mediagrupo_alunos)

#   grupo_alunos2   n   Portuguese  Math   
#
#   1               2   7.50        9.50
#   2               4   5.75        4.75

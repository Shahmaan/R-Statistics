library(tidyverse)
library(dplyr)
library(stringr)

mast_data <- read.csv('inlämningsuppgift/telemastdata.csv')
source("inlämningsuppgift/triangulation.r", encoding = "latin1")

velocity <- 3*10^8

suspect <- mast_data %>%
  group_by(name, phone_type) %>% 
  filter(phone_type == 'iPhone', time0 >= 407 & time0 <= 425)
suspect

# Skapar en kopia av suspect
suspectOriginal <- suspect
rownames(suspectOriginal) <- c('suspect 1', 'suspect 2', 'suspect 3',
                               'suspect 4', 'suspect 5', 'suspect 6')

suspect$'timem1-time0' <- suspect$timem1 - suspect$time0
suspect$'timem2-time0' <- suspect$timem2 - suspect$time0
suspect$'timem3-time0' <- suspect$timem3 - suspect$time0
suspect <- suspect[,7:9] * velocity

suspect <- as.matrix(t(suspect))
master <- matrix(c(4,7,6,4,6,6), ncol = 2, byrow = TRUE)

for(i in 1:6) { 
  master <- cbind(master, suspect[,i])
  susCord <- paste("susCord", i, sep = "")
  assign(susCord, circle_intersection(master))
  master <- master[,-3]
}
# Redan här ser vi att susCord5 = Morritz Hempel är mördaren.

susCords <- c(susCord1, susCord2, susCord3, susCord4, susCord5, susCord6)

susCords <- as.data.frame(susCords)

susCords <- str_split_fixed(susCords$susCords, ", ", 2)

# måste sätta in min susCords på en ny data.frame eftersom annars är coden
# atomic vilket gör så att jag inte kan omvandla mina siffror från
# character till numeric.
is.atomic(susCords)
susCords <- as.data.frame(susCords)

# gör V1 och V2 till numeric.
susCords$V1 <- as.numeric(susCords$V1)
susCords$V2 <- as.numeric(susCords$V2)

# avrundar talen till hundradelar.
susCords$V1 <- round(susCords$V1, digits = 2)
susCords$V2 <- round(susCords$V2, digits = 2)

# ändrar benämningen på kolumner och rader.
colnames(susCords) <- c('x', 'y')
rownames(susCords) <- c('suspect 1', 'suspect 2', 'suspect 3', 'suspect 4',
                        'suspect 5', 'suspect 6')

suspectFinal <- cbind(suspectOriginal, susCords)
suspectFinal

# Mordplats skedde i X = 2.22, Y = 2.92
if(i <- which(suspectFinal$x == 2.22 & suspectFinal$y == 2.92, arr.ind=TRUE)){
  suspectFinal[i,]
}

# Svar: Mördaren är Morritz Hempel


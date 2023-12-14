library("readr")
setwd("/Users/alexandernizov/Desktop")

data <- read.csv("question_10.csv",header = T)
head(data)

library(xts)
library(markovchain)
library(depmixS4)

set.seed(100)

S <- data[,1]
r <- S[2:length(S)] / S[1:length(S)-1] - 1 # ряд доходностей
head(r)

# Create and fit the Hidden Markov Model (Скрытая марковская модель HММ)
hmm <- depmix(r ~ 1, family = gaussian(), nstates = 3, data=data.frame(r=r))
hmmfit <- fit(hmm, verbose = FALSE)
hmm <- markovchainFit(data = hmmfit@posterior$state, method = "mle", name = "Alofi")

trmatr <- hmm$estimate@transitionMatrix # переходные вероятности

States <- c("1", "2", "3")
colnames(trmatr) <- States
rownames(trmatr) <- States

trmatr <- as.matrix(trmatr)
trmatr

steady_states = steadyStates(mc) # cтационарное распределение вероятностей
steady_states

result <- list(
  r, # доходности
  trmatr = trmatr, #матрица переходных вероятностей
  steady_states = steady_states #стационарное распределение вероятностей
)

head(result)

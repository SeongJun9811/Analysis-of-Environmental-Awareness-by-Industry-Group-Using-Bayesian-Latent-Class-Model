install.packages('agricolae')
library(ggplot2)
library(BayesLCA)
library(dplyr)
en <- read.csv('C:/Users/seongjun/Desktop/대학원/환경아이디어/environment.csv', header = T)

sum(table(en$E13))
# 산업직군의 빈도를 계산
f2_freq <- table(en$F2)

# 1차, 2차, 3차 산업 직군으로 분류
ind1 <- en[en$F2 == 1, ]
ind2 <- en[en$F2 == 2, ]
ind3 <- en[en$F2 ==3, ]

# 변수 선택
ind1 <- ind1[,c('E13','E14','E1','B4_3','B4_8','B4_13','B4_6')]
ind2 <- ind2[,c('E13','E14','E1','B4_3','B4_8','B4_13','B4_6')]
ind3 <- ind3[,c('E13','E14','E1','B4_3','B4_8','B4_13','B4_6')]

# 무응답 제거
ind1 <- ind1[!apply(ind1, 1, function(row) any(row == 9)), ]
ind2 <- ind2[!apply(ind2, 1, function(row) any(row == 9)), ]
ind3 <- ind3[!apply(ind3, 1, function(row) any(row == 9)), ]

# 이진변수 변환
ind1[ind1 == 2] <- 0

ind2[ind2 == 2] <- 0

ind3[ind3 == 2] <- 0

en1 <- en[!apply(en, 1, function(row) any(row == 9)), ]

table(en1$E13)/sum(table(en1$E13))
table(en1$E1)/sum(table(en1$E1))
table(en1$E14)/sum(table(en1$E14))
table(en1$B4_3)/sum(table(en1$B4_3))
table(en1$B4_6)/sum(table(en1$B4_6))
table(en1$B4_8)/sum(table(en1$B4_8))
table(en1$B4_13)/sum(table(en1$B4_13))


str(ind1)
str(ind2)
str(ind3)
################################################################################
set.seed(2024)
# 베이지안 잠재계층모형 적용(Bayesian latent class model)
# 1차 산업
ind1_blca2 <- blca.gibbs(ind1, G = 2, alpha = 1, beta = 1, delta = 1,
                        start.vals = c("prior"),
                        counts.n = NULL, iter = 5000, thin = 1,
                        accept=0.3, burn.in = 2000, relabel = TRUE
)

ind1_blca3 <- blca.gibbs(ind1, G = 3, alpha = 1, beta = 1, delta = 1,
                        start.vals = c("prior"),
                        counts.n = NULL, iter = 5000, thin = 1,
                        accept=0.3, burn.in = 2000, relabel = TRUE
)

ind1_blca4 <- blca.gibbs(ind1, G = 4, alpha = 1, beta = 1, delta = 1,
                        start.vals = c("prior"),
                        counts.n = NULL, iter = 5000, thin = 1,
                        accept=0.3, burn.in = 2000, relabel = TRUE
)

# grop1 = red, group = green
plot(ind1_blca2, which = 5)

ind1_blca2$DIC
ind1_blca3$DIC
ind1_blca4$DIC
################################################################################
# 2차 산업
ind2_blca2 <- blca.gibbs(ind2, G = 2, alpha = 1, beta = 1, delta = 1,
                         start.vals = c("prior"),
                         counts.n = NULL, iter = 5000, thin = 1,
                         accept=0.3, burn.in = 2000, relabel = TRUE
)

ind2_blca3 <- blca.gibbs(ind2, G = 3, alpha = 1, beta = 1, delta = 1,
                         start.vals = c("prior"),
                         counts.n = NULL, iter = 5000, thin = 1,
                         accept=0.3, burn.in = 2000, relabel = TRUE
)

ind2_blca4 <- blca.gibbs(ind2, G = 4, alpha = 1, beta = 1, delta = 1,
                         start.vals = c("prior"),
                         counts.n = NULL, iter = 5000, thin = 1,
                         accept=0.3, burn.in = 2000, relabel = TRUE
)

# grop1 = red, group = green
plot(ind2_blca2, which = 5)

ind2_blca2$DIC
ind2_blca3$DIC
ind2_blca4$DIC


################################################################################
# 3차 산업
ind3_blca2 <- blca.gibbs(ind3, G = 2, alpha = 1, beta = 1, delta = 1,
                         start.vals = c("prior"),
                         counts.n = NULL, iter = 5000, thin = 1,
                         accept=0.3, burn.in = 2000, relabel = TRUE
)

ind3_blca3 <- blca.gibbs(ind3, G = 3, alpha = 1, beta = 1, delta = 1,
                         start.vals = c("prior"),
                         counts.n = NULL, iter = 5000, thin = 1,
                         accept=0.3, burn.in = 2000, relabel = TRUE
)

ind3_blca4 <- blca.gibbs(ind3, G = 4, alpha = 1, beta = 1, delta = 1,
                         start.vals = c("prior"),
                         counts.n = NULL, iter = 5000, thin = 1,
                         accept=0.3, burn.in = 2000, relabel = TRUE
)

# grop1 = red, group = green
plot(ind3_blca2, which = 5)

ind3_blca2$DIC
ind3_blca3$DIC
ind3_blca4$DIC

###############################################################################
#분산분석
#전처리
ind1_cp <- ind1_blca2$samples$classprob[,2]
ind2_cp <- ind2_blca2$samples$classprob[,2]
ind3_cp <- ind3_blca2$samples$classprob[,2]

# 데이터프레임 생성
data <- data.frame(
  group = factor(c(rep("ind1", length(ind1_cp)), rep("ind2", length(ind2_cp)), rep("ind3", length(ind3_cp)))),
  classprob = c(hh1_cp, hh2_cp, hh3_cp)
)

# ANOVA 수행
anova_result <- aov(classprob ~ group, data = data)

# ANOVA 결과 출력
summary(anova_result)

#ANOVA 검정 결과가 유의미하므로 이에 따른 사후검정
library(agricolae)
duncan <- duncan.test(anova_result, "group")
duncan









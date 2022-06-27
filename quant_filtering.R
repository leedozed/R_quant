# install.packages("quantmod")
# install.packages("PerformanceAnalytics")
# install.packages("magrittr")


library(quantmod)
library(PerformanceAnalytics)
library(magrittr)

symbols = c('102110.KS', '039490.KS')
getSymbols(symbols)

prices = do.call(cbind,
                 lapply(symbols, function(x)Cl(get(x))))

ret = Return.calculate(prices)
ret = ret['2016-01::2018-12']

rm = ret[,1]
ri = ret[,2]

#회귀분석 진행
reg = lm(ri ~ rm)
summary(reg)

#pch: 점의 모양, cex: 점의 크기
#xlim: 최대 범위 지정
plot(as.numeric(rm), as.numeric(ri), pch = 4, cex=0.3,
     xlab = "KOSPI 200", ylab = "Individual Stock",
     xlim = c(-0.02, 0.02), lyt =2)

#a: 상수, b: 기울기, lty: 선의유형
abline(a = 0, b = 1, lty = 2)
abline(reg, col = 'red')

#저변동성 전략
example = c(85, 76, 73, 80, 72)
sd(example)

library(stringr)
library(xts)
library(PerformanceAnalytics)
library(magrittr)
library(ggplot2)
library(dplyr)

# F:\quant_cookbook-master

KOR_price = read.csv('F:/quant_cookbook-master/data/KOR_price.csv', row.names = 1,
                     stringsAsFactors = FALSE) %>% as.xts()

KOR_ticker = read.csv('F:/quant_cookbook-master/data/KOR_ticker.csv', row.names = 1,
                      stringsAsFactors = FALSE) 

KOR_ticker$'종목코드' = 
  str_pad(KOR_ticker$'종목코드', 6, 'left', 0)

ret = Return.calculate(KOR_price)

std_12m_daily = xts::last(ret, 252) %>% apply(., 2, sd) %>%
  multiply_by(sqrt(252))

std_12m_daily %>% 
  data.frame() %>%
  ggplot(aes(x = (`.`))) +
  geom_histogram(binwidth = 0.01) +
  annotate("rect", xmin = -0.02, xmax = 0.02,
           ymin = 0,
           ymax = sum(std_12m_daily == 0, na.rm = TRUE) * 1.1,
           alpha=0.3, fill="red") +
  xlab(NULL)

std_12m_daily[std_12m_daily == 0] = NA

std_12m_daily[rank(std_12m_daily) <= 30]
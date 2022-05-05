根据《环境空气质量指数（AQI）技术规定（HJ 633—2012）》的相关标准和方法，提供五个用于计算AQI的函数，分别为：
- IAQI_hourly(Pollu,Conc)
  - 用于计算时均空气质量分指数（individual air quality index，即IAQI）
- AQI_Hourely(SO2,NO2,CO,O3)
  - 用于计算时均空气污染综合指数（air quality index， 即AQI）
- DaliyMeanConc(Conc)
  - 根据24小时检测浓度求日均浓度
- IAQI_Daily(Pollu,Conc)
  - 用于计算日均AQI分指数
- AQI_Daily(SO2,NO2,CO,PM10,PM2.5,O3)
  - 用于计算日均AQI计算

# 用法
## 1. 载入函数及基础用法

```{r}
source('AQI.R',encoding = 'UTF8')
# -- Attaching packages ----------------------------------------------------------------------- tidyverse 1.3.1 --
# v ggplot2 3.3.5     v purrr   0.3.4
# v tibble  3.1.6     v dplyr   1.0.8
# v tidyr   1.2.0     v stringr 1.4.0
# v readr   2.1.2     v forcats 0.5.1
# -- Conflicts -------------------------------------------------------------------------- tidyverse_conflicts() --
# x dplyr::filter() masks stats::filter()
# x dplyr::lag()    masks stats::lag()
# 载入需要的程辑包：zoo
# 
# 载入程辑包：‘zoo’
# 
# The following objects are masked from ‘package:base’:
# 
#     as.Date, as.Date.numeric
```

```{r}
c(
  IAQI_SO2 = IAQI_hourly('SO2',300),
  IAQI_NO2 = IAQI_hourly('NO2',200),
  IAQI_CO = IAQI_hourly('CO',100),
  IAQI_O3 = IAQI_hourly('O3',120)
)
# IAQI_SO2  IAQI_NO2   IAQI_CO   IAQI_O3 
# 71.42857 100.00000 333.33333  37.50000 
```

```{r}
AQI_Hourely(SO2 = 300,NO2 = 200,CO = 100,O3 = 120)
# [1] 333.3333
```

日均AQI及IAQI的计算方法类似，不重复了。

# 批量计算示范
## 1. 构建实例数据：每日6种主要污染物的逐小时检测值。
```{r}
sampleConc <- data.frame(
  Time = 0:23,
  SO2 = rnorm(24,mean = 300,sd = 20),
  NO2 = rnorm(24,mean = 800, sd = 100),
  O3 = rnorm(24,mean = 500,sd = 100), 
  CO = rnorm(24,mean = 40,sd = 10),
  PM2.5 = rnorm(24, mean = 200, sd =50),
  PM10 = rnorm(24,mean = 350,sd = 100)
)

sampleConc
#     Time      SO2      NO2       O3       CO    PM2.5     PM10
# 1     0 312.0985 869.2471 565.8239 46.80609 216.2358 352.8107
# 2     1 324.9681 806.9140 369.0104 45.03742 208.2282 311.6855
# 3     2 299.2635 916.7897 438.5795 61.40355 236.3040 431.6210
# 4     3 299.2541 695.2306 387.8582 29.36881 302.3207 245.0134
# 5     4 278.2592 857.3538 542.3937 22.11222 235.1808 387.6461
# 6     5 312.0856 716.6027 379.9761 43.71543 214.6801 413.1603
# 7     6 333.1950 704.8249 540.2819 48.72006 118.5922 265.0442
# 8     7 328.3054 822.6588 524.3190 52.58529 252.9043 359.7584
# 9     8 298.6037 998.6824 676.5627 42.27579 194.0580 368.9174
# 10    9 317.8211 830.7829 449.5631 35.28967 225.3505 426.8057
# 11   10 282.6243 662.0513 504.5944 46.31453 219.7618 393.4738
# 12   11 302.8119 847.3821 385.7167 33.03166 235.3936 317.6218
# 13   12 320.6861 729.6520 286.9293 39.94401 234.9105 431.9434
# 14   13 303.9984 809.2879 608.3109 26.28806 219.0930 520.0152
# 15   14 298.3911 867.7495 336.1087 35.77609 249.5489 349.9614
# 16   15 297.9970 669.5779 702.4120 26.20081 174.6598 412.8197
# 17   16 288.2577 615.2613 507.7221 51.52015 234.0276 293.5287
# 18   17 292.8574 671.9441 545.6387 26.42969 246.1632 257.6705
# 19   18 275.5850 814.6357 728.2495 51.44869 225.0670 283.1428
# 20   19 314.1641 894.2051 685.6261 29.34085 193.6137 238.5211
# 21   20 281.0737 843.0278 638.0354 41.47885 247.3289 426.7471
# 22   21 312.6405 798.5428 405.3461 46.05751 194.6194 369.8122
# 23   22 323.3640 708.7130 313.9148 26.71198 167.0441 432.7830
# 24   23 286.4532 728.1761 475.8784 35.99647 235.1535 384.0995
```

## 2.求小时IAQI及AQI：
```{r}
c('SO2', 'NO2', 'O3', 'CO') %>% set_names() %>% map_dfc(
  function(Pollu) sampleConc %>% rowwise %>% mutate(across(all_of(Pollu), ~ IAQI_hourly(Pollu,.x)),.keep = 'none')
) %>% rename_with(~paste0(.x,'_IAQI_h'))
# # A tibble: 24 x 4
# # Rowwise: 
#    SO2_IAQI_h NO2_IAQI_h O3_IAQI_h CO_IAQI_h
#         <dbl>      <dbl>     <dbl>     <dbl>
#  1       73.2       167.      171.      174.
#  2       75.0       161.      142.      170.
#  3       71.3       172.      155.      205.
#  4       71.3       150.      147.      139.
#  5       68.3       166.      168.      124.
#  6       73.2       152.      145.      167.
#  7       76.2       150.      168.      177.
#  8       75.5       162.      166.      185.
#  9       71.2       180.      185.      165.
# 10       74.0       163.      156.      151.
# # ... with 14 more rows
```
```{r}
sampleConc %>% rowwise %>% 
  mutate(AQI_h = AQI_Hourely(SO2 = SO2,NO2 = NO2,CO = CO,O3 = O3), .keep = 'none')
# # A tibble: 24 x 1
# # Rowwise: 
#    AQI_h
#    <dbl>
#  1  174.
#  2  170.
#  3  205.
#  4  150.
#  5  168.
#  6  167.
#  7  177.
#  8  185.
#  9  185.
# 10  163.
# # ... with 14 more rows
```

## 3. 求日均AQI及IAQI
```{r}
names(sampleConc) %>% .[-1] %>% set_names() %>% map_dfc(
  function(Pollu) sampleConc %>% DaliyMeanConc %>% .[[Pollu]] %>% map_dbl( ~ IAQI_Daily(Pollu,.x))
) %>% rename_with(~paste0(.x,'_IAQI_d'))
# # A tibble: 1 x 6
#   SO2_IAQI_d NO2_IAQI_d O3_IAQI_d CO_IAQI_d PM2.5_IAQI_d PM10_IAQI_d
#        <dbl>      <dbl>     <dbl>     <dbl>        <dbl>       <dbl>
# 1       124.       419.      261.      328.         270.        216.
```
```{r}
sampleConc %>% DaliyMeanConc %>%
  mutate(AQI_d = AQI_Daily(SO2 = SO2,NO2 = NO2,CO = CO,O3 = O3,PM10 = PM10 ,PM2.5 = PM2.5), .keep = 'none')
#      AQI_d
# 1 419.2828
```


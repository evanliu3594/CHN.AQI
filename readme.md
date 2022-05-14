根据中华人民共和国生态环境部《环境空气质量指数（AQI）技术规定（HJ 633—2012）》中的相关标准和方法，提供五个用于计算AQI的函数，分别为：
- IAQI_hourly(Pollu,Conc)
  - 用于计算时均空气质量分指数（individual air quality index，即IAQI）
- AQI_Hourely(SO2,NO2,CO,O3)
  - 用于计算时均空气污染综合指数（air quality index， 即AQI）
- DaliyMeanConc(Conc)
  - 根据24小时监测数据浓度求各污染物日均浓度，其中臭氧为最大八小时平均浓度
- IAQI_Daily(Pollu,Conc)
  - 用于计算日均IAQI
- AQI_Daily(SO2,NO2,CO,PM10,PM2.5,O3)
  - 用于计算日均AQI

# 用法
## 1. 基本用法

```{r}
> source('AQI.R',encoding = 'UTF8')

载入程辑包：‘dplyr’

The following objects are masked from ‘package:stats’:

    filter, lag

The following objects are masked from ‘package:base’:

    intersect, setdiff, setequal, union


载入程辑包：‘zoo’

The following objects are masked from ‘package:base’:

    as.Date, as.Date.numeric
```

```{r}
> c(
    IAQI_SO2 = IAQI_hourly('SO2',300),
    IAQI_NO2 = IAQI_hourly('NO2',200),
    IAQI_CO = IAQI_hourly('CO',100),
    IAQI_O3 = IAQI_hourly('O3',120)
  )
IAQI_SO2  IAQI_NO2   IAQI_CO   IAQI_O3 
71.42857 100.00000 333.33333  37.50000 
```

```{r}
> AQI_Hourely(SO2 = 300,NO2 = 200,CO = 100,O3 = 120)
[1] 333.3333
```

日均AQI及IAQI的计算方法类似，不再重复了。

# 批量计算示范
## 1. 构建实例数据：每日6种主要污染物的逐小时检测值。
```{r}
> sampleConc <- data.frame(
    Time = 0:23,
    SO2 = rnorm(24,mean = 300,sd = 20),
    NO2 = rnorm(24,mean = 800, sd = 100),
    O3 = rnorm(24,mean = 500,sd = 100), 
    CO = rnorm(24,mean = 40,sd = 10),
    PM2.5 = rnorm(24, mean = 200, sd =50),
    PM10 = rnorm(24,mean = 350,sd = 100)
  )

> sampleConc

   Time      SO2       NO2       O3       CO    PM2.5     PM10
1     0 316.9730  997.1874 640.9813 61.44341 144.3764 462.8190
2     1 301.1342  728.9767 561.3807 38.34045 248.5040 341.6524
3     2 302.0868  671.3171 584.8845 51.07946 194.3588 332.0028
4     3 299.9343  804.2640 460.5617 37.05063 260.5968 420.0639
5     4 298.1825  598.3578 492.6676 37.36674 232.9466 501.1301
6     5 327.1345  780.0082 553.5663 32.05783 179.4785 431.2695
7     6 274.9260  833.1178 503.2564 37.48729 203.1519 430.6763
8     7 350.1049 1016.0101 357.9588 39.97116 146.6490 470.7740
9     8 276.8420  772.2998 461.9703 53.59496 193.8068 145.6300
10    9 270.9210  799.8001 466.1902 40.28676 234.3580 543.2716
11   10 312.5188  853.5581 365.5725 36.89271 124.1807 466.1997
12   11 323.6585  862.7200 556.0066 32.07788 167.4295 430.8826
13   12 288.1699  765.4171 474.5379 31.68711 216.6561 228.5781
14   13 308.5859  694.6297 329.2448 20.41216 211.9273 486.5773
15   14 281.2515  720.6994 576.4883 35.99295 268.0571 247.0022
16   15 327.3003  865.8750 479.8326 35.64703 167.4294 214.0925
17   16 302.5222  829.1313 460.6093 51.89485 229.9200 348.0282
18   17 291.3967  705.1757 523.3486 45.35436 155.3770 280.2555
19   18 293.2497  769.4859 528.1477 38.88545 196.2277 418.5855
20   19 328.2578  846.6787 393.4523 50.27464 161.2237 305.2331
21   20 256.3332  848.9420 506.4515 57.07515 260.5609 479.9673
22   21 309.9392  772.1076 462.5124 49.68822 172.5458 221.0516
23   22 300.1342  788.2577 574.3972 34.15167 261.2344 358.6539
24   23 316.3513  813.2033 590.1278 25.57155 292.8286 258.2533
```

## 2.求小时IAQI及AQI：
```{r}
> sampleConc |>
    pivot_longer(-Time,names_to = 'Pollu',values_to = 'Conc') |> 
    mutate(IAQI_h = map2_dbl(.x = Pollu, .y  = Conc, ~IAQI_hourly(.x, .y))) |>
    select(- Conc) |> pivot_wider(names_from = 'Pollu', values_from = 'IAQI_h')

# A tibble: 24 x 7
    Time   SO2   NO2    O3    CO PM2.5  PM10
   <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
 1     0  73.9  180.  180.  205.    NA    NA
 2     1  71.6  153.  170.  157.    NA    NA
 3     2  71.7  147.  173.  182.    NA    NA
 4     3  71.4  160.  158.  154.    NA    NA
 5     4  71.2  140.  162.  155.    NA    NA
 6     5  75.3  158.  169.  144.    NA    NA
 7     6  67.8  163.  163.  155.    NA    NA
 8     7  78.6  182.  139.  160.    NA    NA
 9     8  68.1  157.  158.  187.    NA    NA
10     9  67.3  160.  158.  161.    NA    NA
# ... with 14 more rows
There were 48 warnings (use warnings() to see them)
```
```{r}
> sampleConc |> rowwise() |> 
     mutate(AQI_h = AQI_Hourely(SO2 = SO2,NO2 = NO2,CO = CO,O3 = O3), .keep = 'none')

# A tibble: 24 x 1
# Rowwise: 
   AQI_h
   <dbl>
 1  205.
 2  170.
 3  182.
 4  160.
 5  162.
 6  169.
 7  163.
 8  182.
 9  187.
10  161.
# ... with 14 more rows
```

## 3. 求日均IAQI及AQI
```{r}
> sample_daily_conc <- sampleConc |>  DaliyMeanConc()
> sample_daily_conc

       SO2      NO2       CO    PM2.5     PM10       O3
1 302.4128 797.3842 40.59518 205.1594 367.6104 640.9813

> sample_daily_conc |>
    pivot_longer(everything(),names_to = 'Pollu',values_to = 'Conc') |> 
    mutate(IAQI_d = map2_dbl(.x = Pollu, .y  = Conc, ~IAQI_Daily(.x, .y))) |> 
    select(- Conc) |> pivot_wider(names_from = 'Pollu', values_from = 'IAQI_d')

# A tibble: 1 x 6
    SO2   NO2    CO PM2.5  PM10    O3
  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
1  123.  425.  338.  255.  225.  270.
```
```{r}
> sample_daily_conc %>%
    mutate(
      AQI_d = AQI_Daily(SO2 = SO2,NO2 = NO2,CO = CO,
                        O3 = O3,PM10 = PM10 ,PM2.5 = PM2.5),
      .keep = 'none'
    )

    AQI_d
1 424.939
```



# dateutils

**dateutils** is an R package for conveniently working with time series data in tabular format, that is, without using one of the may R time series formats such as `ts` or `xts`. `dateutils` is built on top of data table.

## Non-Technical Overview

The main functions of **dateutils** are for aggregating and formatting time series data. Advanced functions are in a separate package built on top of **dateutils** called **dataprocess**. 

## Getting Started

To give a few examples of using **dateutils**, we will use the built in data.table named `fred`, containing daily (t bill sreads), weekly (intitial jobess claims), monthly (advance retail sales), and quarterly (gdp) data in long format. To view the data use `View(fred)`. We can begin by aggregating this data to the lowest frequency, quarterly in this case. 


```{r}
library(dateutils)
library(data.table)
fred_quarterly <- agg_to_freq(fred, frq = "quarter")
print(fred_quarterly[1:5])
```

Note that dates are indexed to the end of the quarter. `agg_to_frq()` also includes the column `n_obs` giving the number of observations which were used to calculate mean values for the quarter. We could, alternatively, create a mixed frequency data set by aggregating to monthly. If we wanted the data in wide format, we could then use the **data.table** function `dcast()`, or simply call `agg_to_frq_wide()`. We will use data since January 1st 2000 since advance retail sales has a shorter history than other series.

```{r}
fred_monthly_wide <- agg_to_freq_wide(fred[ref_date >= as.Date("2000-01-01")], frq = "month")
print(fred_monthly_wide$dt[1:12])
```

## Seasonal Adjustment

**dateutils** works with the package **seasonal** to seasonally adjust monthly and quarterly data in `data.table` format. For example, suppose we wanted to seasonally adjust gdp and advance retail sales in our quarterly dataset. We can do so as follows:

```{r}
fred_sa <- seas_df_long(fred_quarterly, sa_names = c("gdp constant prices", "advance retail sales"), transfunc = 'auto')
gdp <- rbind(fred_quarterly[series_name == "gdp constant prices", .(ref_date, series_name, value)],
             fred_sa$values_sa[series_name == "gdp constant prices sa"])
gdp <- dcast(gdp, ref_date ~ series_name, value.var = "value")
matplot(gdp$ref_date, gdp[,-1,with=FALSE], type = 'l')
```

## Add this line just to test if I can succeed in making changes to the code and commit to the branch
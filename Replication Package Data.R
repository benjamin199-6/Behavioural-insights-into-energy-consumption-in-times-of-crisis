


library(tidyverse)
library(readxl)
library(lubridate)

# ===============================================================
# 1. Load original data (wide format)
# ===============================================================

wide <- read_excel("Online interventions database_EFSA_June'23.xlsx")

# ===============================================================
# 2. Wide → long (Consumption / Invoice / Price by month)
# ===============================================================

df_long <- wide %>%
  pivot_longer(
    cols = starts_with("Consumption") |
      starts_with("Invoice value") |
      starts_with("Price"),
    names_to  = "Type",
    values_to = "Value"
  )

# ===============================================================
# 3. Extract month-year and variable type from column names
# ===============================================================

df_long$Date <- gsub(".*\\s(\\w+'\\d+).*", "\\1", df_long$Type)
df_long$Type <- gsub("\\s\\w+'\\d+", "", df_long$Type)

# ===============================================================
# 4. Long → wide (one row per unit × month)
# ===============================================================

df_long <- df_long %>%
  pivot_wider(
    id_cols = c(
      Nr., `Intervention type`, `Type of meter reading`,
      PROVINCE, COUNTY, CITY,
      AGE, `AGE RANGES`, GENDER, Date
    ),
    names_from  = Type,
    values_from = Value
  )

# ===============================================================
# 5. Construct Date variable from "Mon'YY"
# ===============================================================

df_long$Date <- gsub("Sept", "Sep", df_long$Date)

month_str <- sub("'.*$", "", df_long$Date)
year_num  <- 2000 + as.integer(sub("^.*'", "", df_long$Date))
month_num <- match(month_str, month.abb)

df_long$Date <- as.Date(
  paste(year_num, month_num, "01", sep = "-")
)

# ===============================================================
# 6. Impute missing prices where possible
# ===============================================================

df_long <- df_long %>%
  mutate(
    `Price (lei/kWh)` = if_else(
      is.na(`Price (lei/kWh)`) &
        !is.na(`Invoice value (lei)`) &
        `Consumption (kWh)` != 0,
      as.numeric(`Invoice value (lei)`) / as.numeric(`Consumption (kWh)`),
      if_else(
        is.na(`Price (lei/kWh)`) &
          !is.na(`Invoice value (lei)`) &
          `Consumption (kWh)` == 0,
        NA_real_,
        `Price (lei/kWh)`
      )
    )
  )

# ===============================================================
# 7. Treatment indicator
# ===============================================================

df_long$treatment <- ifelse(
  df_long$`Intervention type` != "Control group" &
    df_long$Date %in% as.Date(c("2022-05-01", "2022-06-01", "2022-07-01")),
  1, 0
)

# ===============================================================
# 8. Flag corrections
# ===============================================================

df_long <- df_long %>%
  mutate(correction = if_else(`Consumption (kWh)` < 0, 1, 0))

# ===============================================================
# 9. Rename variables
# ===============================================================

df_long <- df_long %>%
  rename(
    id           = `Nr.`,
    county       = COUNTY,
    gender       = GENDER,
    groups       = `Intervention type`,
    city         = CITY,
    date         = Date,
    age          = AGE,
    consumption  = `Consumption (kWh)`,
    invoice      = `Invoice value (lei)`,
    price        = `Price (lei/kWh)`,
    province     = PROVINCE,
    type_reading = `Type of meter reading`
  )

# ===============================================================
# 10. Basic transformations
# ===============================================================

df <- df_long
rm(df_long)

df$female <- ifelse(df$gender == "female", 1, 0)

df <- df %>%
  mutate(groups = case_when(
    groups == "Intervention 1"   ~ "individual economic",
    groups == "Intervention 2"   ~ "collective environment",
    groups == "Intervention 3.1" ~ "individual health",
    groups == "Intervention 3.2" ~ "collective health",
    groups == "Control group"    ~ "control group"
  ))

# ===============================================================
# 11. Pre / post split
# ===============================================================

intervention_start <- as.Date("2022-05-01")
df$post <- ifelse(df$date >= intervention_start, 1, 0)

# ===============================================================
# 12. Handle negative consumption
# ===============================================================

df <- df %>%
  mutate(consumption_positive = if_else(consumption < 0, NA_real_, consumption))

# ===============================================================
# 13. Percentiles / deciles
# ===============================================================

df <- df %>%
  mutate(
    consumption_percentile = ntile(consumption_positive, 100),
    consumption_decile = cut(
      consumption_percentile,
      breaks = seq(0, 100, by = 20),
      include.lowest = TRUE,
      labels = FALSE
    )
  )

# ===============================================================
# 14. Drop households with corrections
# ===============================================================

ids_with_corrections <- df %>% filter(consumption < 0)
df <- df %>% filter(!id %in% unique(ids_with_corrections$id))

# ===============================================================
# 15. Group × post interaction
# ===============================================================

df$group_post <- interaction(df$post, df$groups)

# ===============================================================
# 16. Outlier removal by yearly totals
# ===============================================================

df$year <- year(df$date)

year1 <- df %>% filter(year == 2020)
year2 <- df %>% filter(year == 2021)
year3 <- df %>% filter(year == 2022)

x1 <- year1 %>% group_by(id) %>% summarise(sum = sum(consumption))
x2 <- year2 %>% group_by(id) %>% summarise(sum = sum(consumption))
x3 <- year3 %>% group_by(id) %>% summarise(sum = sum(consumption))

Q1 <- quantile(x1$sum, 0.25)
Q3 <- quantile(x1$sum, 0.75)
IQR <- Q3 - Q1
x1 <- x1 %>% filter(sum >= Q1 - 1.5 * IQR & sum <= Q3 + 1.5 * IQR)

Q1 <- quantile(x2$sum, 0.25)
Q3 <- quantile(x2$sum, 0.75)
IQR <- Q3 - Q1
x2 <- x2 %>% filter(sum >= Q1 - 1.5 * IQR & sum <= Q3 + 1.5 * IQR)

Q1 <- quantile(x3$sum, 0.25)
Q3 <- quantile(x3$sum, 0.75)
IQR <- Q3 - Q1
x3 <- x3 %>% filter(sum >= Q1 - 1.5 * IQR & sum <= Q3 + 1.5 * IQR)

ID1 <- unique(x1$id)
ID2 <- unique(x2$id)
ID3 <- unique(x3$id)

df2=df %>% dplyr::filter(id %in% ID1)
df2=df2 %>% dplyr::filter(id %in% ID2) 
df2=df2 %>% dplyr::filter(id %in% ID3)
df=df2
rm(list = setdiff(ls(), c("wide", "df")))

# ===============================================================
# 17. Drop extreme monthly consumption
# ===============================================================

df2 <- df %>% filter(consumption > 2000)
df <- df %>% filter(!id %in% unique(df2$id))

# ===============================================================
# 18. Treatment dummies and logs
# ===============================================================

df <- df %>%
  mutate(
    treatment1 = ifelse(groups == "individual economic", 1, 0),
    treatment2 = ifelse(groups == "collective environment", 1, 0),
    treatment3 = ifelse(groups == "individual health", 1, 0),
    treatment4 = ifelse(groups == "collective health", 1, 0),
    control    = ifelse(groups == "control group", 1, 0),
    log_consumption = log(consumption + 1)
  )


df=df %>% filter(consumption>=1) ### for log 
data.table::fwrite(df, "ERSS_Kirchler_et_al_2024_paper.csv")

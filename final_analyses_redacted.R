##########################################
### Analysing Advertising Revenue Data ###
##########################################


# Note: Script has had all local authority information redacted as per our data sharing agreement. The order of 'redacted' is not the same as 'LA' in the paper either and dates have been scrambled too. 

# Libraries
library(readxl)
library(data.table)
library(lubridate)
library(fixest)
library(ggplot2)
library(metafor)
library(sandwich)


## Load and tidy data ##


# redacted1 #

# Load
redacted1 <- read_excel("./redacted1.xlsx", col_names = TRUE)
setDT(redacted1)

# Create date
redacted1[, date := as.Date(paste(year, month, "1"), format = "%Y %B %d")]

# Create local authority variable
redacted1$la <- "redacted1"

# Clean names
setnames(redacted1, old  = "total", new = "revenue")

# Delete variables not required
redacted1 <- redacted1[, c("la", "date", "revenue")]


# redacted2 #

# Load
redacted2 <- read_excel("redacted2.xlsx", col_names = TRUE)
setDT(redacted2)

# Create date
redacted2[, date := as.Date(paste(year, month, "1"), format = "%Y %B %d")]

# Create local authority variable
redacted2$la <- "redacted2"

# Delete variables not required
redacted2 <- redacted2[, c("la", "date", "revenue")]


# redacted2 - brand events only #

# Load
redacted2_brand <- read_excel("./redacted2_brand.xlsx", col_names = TRUE)
setDT(redacted2_brand)

# Create date
redacted2_brand[, date := as.Date(paste(year, month, "1"), format = "%Y %B %d")]

# Subset total advertising revenue
redacted2_brand_total <- redacted2_brand[, c("date", "Total")] # Subset
setnames(redacted2_brand_total, old  = "Total", new = "revenue") # Rename
redacted2_brand_total$la <- "redacted2 Brand Total" # Create local authority variable

# Subset only food and drink advertising revenue (38% of total advertising revenue was under food and drink)
redacted2_brand_food <- redacted2_brand[, c("date", "Food_and_Drink")] # Subset
setnames(redacted2_brand_food, old  = "Food_and_Drink", new = "revenue") # Rename
redacted2_brand_food$la <- "redacted2 Brand Food" # Create local authority variable



# redacted3 #

# Load 
redacted3_raw <- read_excel("redacted3.xlsx", col_names = FALSE)
setDT(redacted3_raw)

# Rename columns
setnames(redacted3_raw, c("year", "month", "roundabouts", "bus", "total"))

# Remove empty columns and rows
redacted3_raw <- redacted3_raw[-c(1, 2), c(1:5)]

# Fill down year (merged cells fix)
redacted3_raw[, year := zoo::na.locf(year)]

# Remove £ formatting (even if hidden) and make numeric
redacted3_raw[, roundabouts := as.numeric(gsub("[^0-9.]", "", roundabouts))]
redacted3_raw[, bus := as.numeric(gsub("[^0-9.]", "", bus))]
redacted3_raw[, total := as.numeric(gsub("[^0-9.]", "", total))]

# Create date
redacted3_raw[, date := as.Date(paste(year, month, "1"), format = "%Y %b %d")]

# Get final dataset structure
redacted3 <- redacted3_raw[, .(
  la = "redacted3",
  date,
  revenue = roundabouts # change to total when clarified the bus shelter issue
)]

# Drop missing rows (here data not available for first six months
redacted3 <- redacted3[!is.na(redacted3$revenue)]

# Tidy
rm(redacted3_raw)


# redacted4 # 

redacted4 <- read_excel("redacted4.xlsx")
setDT(redacted4)

# Remove last row as this is a total
redacted4 <- redacted4[-c(47)]

# Clean names
setnames(redacted4, c("date", "revenue"))

# Convert date
redacted4[, date := as.Date(as.numeric(date), origin = "1899-12-30")]

# Ensure numeric
redacted4[, revenue := as.numeric(revenue)]

# How to handle negative values - make positive as error
redacted4[revenue < 0, revenue := abs(revenue)]

# Add LA
redacted4[, la := "redacted4"]


# Combine into analysis ready format # 

# Combine data
df <- rbind(redacted3, redacted4, redacted1, redacted2, redacted2_brand_total, redacted2_brand_food) # Join objects together
setorder(df, la, date) # Set order

# Define policy dates
policy_dates <- data.table(
  la = c("redacted3", "redacted4", "redacted1", "redacted2", "redacted2 Brand Total", "redacted2 Brand Food"),
  policy_date = as.Date(c("date", "date", "date", "date", "date", "date"))
)
# At the moment this is defined based on when introduced/votes through (policy intent), but we might want to also do this as from when the contracts formally changed for main analysis) as the actual exposure - we also do from when announced as sensitivity too for two which did. 
df <- merge(df, policy_dates, by = "la", all.x = TRUE) # Join on policy dates

# Tidy up data
df[, event_time := interval(policy_date, date) %/% months(1)] # Create event time
df[, post := event_time >= 0] # Define period after intervention
df[, log_revenue := log(revenue)] # Create logged value of revenue so can get % changes
setorder(df, la, date) # Change order for next line
df[, time := 1:.N, by = la] # Create time variable

# Create indexed value
# Note: Some LAs have a wide month-to-month volatility, so selecting a single time point creates distortion as noisy (e.g., could be really high or low) - so used alternative below
baseline <- df[event_time %in% -3:-1, # For last 6 months before intervention
               .(base_revenue = mean(revenue, na.rm = TRUE)), # Calculate mean value by local authority
               by = la]
df <- merge(df, baseline, by = "la", all.x = TRUE) # Join above onto main data to allow next step
df[, revenue_index := (revenue / base_revenue) * 100] # Create indexed value
df[, log_index := log(revenue / base_revenue)] # Alternative to above



## Interrupted time series model ##

# Create function to run models for each local authority seperately
run_its <- function(df,
                    outcome = "log_revenue",
                    coef_name = "postTRUE") {

  effects <- df[, {

    f <- as.formula(
      paste0(
        outcome,
        " ~ time + post + time:post"
      )
    )

    m <- feols(
      f,
      data = .SD,
      vcov = NW(3)
    )

    beta <- coef(m)[coef_name]
    se <- sqrt(vcov(m)[coef_name, coef_name])

    .(
      beta = beta,
      se = se
    )

  }, by = la]

  effects[, `:=`(
    pct_change = (exp(beta) - 1) * 100,
    pct_ci_lower = (exp(beta - 1.96 * se) - 1) * 100,
    pct_ci_upper = (exp(beta + 1.96 * se) - 1) * 100
  )]

  return(effects)

}

# Run model but exclude branded events 
main_ads <- df[
  !la %in% c(
    "redacted2 Brand Total",
    "redacted2 Brand Food"
  )
]

# Store results
its_results <- run_its(main_ads)
its_results # Print results

# Run meta-analysis step with random effects
reml_model <- rma(
  yi = beta,
  sei = se,
  method = "REML",
  data = its_results
)

summary(reml_model)

# Get pooled results
pooled_reml <- data.table(

  la = "Pooled (REML)",

  beta = as.numeric(reml_model$b),

  se = reml_model$se

)

pooled_reml[, `:=`(

  pct_change =
    (exp(beta) - 1) * 100,

  pct_ci_lower =
    (exp(beta - 1.96 * se) - 1) * 100,

  pct_ci_upper =
    (exp(beta + 1.96 * se) - 1) * 100

)]

results_final <- rbind(
  its_results,
  pooled_reml,
  fill = TRUE
)

results_final

# Heterogenity 
cat(
  "Tau² =", round(reml_model$tau2, 4),
  "\nI² =", round(reml_model$I2, 1), "%"
)

# Forest plot

forest_plot_df <- copy(its_results)

ggplot(
  forest_plot_df,
  aes(
    x = pct_change,
    y = reorder(la, pct_change)
  )
) +
  geom_point(size = 3) +
  geom_errorbarh(
    aes(
      xmin = pct_ci_lower,
      xmax = pct_ci_upper
    ),
    height = 0.15
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed"
  ) +
  labs(
    x = "% change in revenue",
    y = NULL,
    title = "Immediate level change following policy implementation"
  ) +
  theme_minimal(base_size = 12)


# Check branded event results
brand_events <- df[
  la %in% c(
    "redacted2 Brand Total",
    "redacted2 Brand Food"
  )
]

brand_results <- run_its(
  brand_events
)

brand_results
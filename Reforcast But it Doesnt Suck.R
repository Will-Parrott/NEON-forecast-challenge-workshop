## install.packages('remotes')
## install.packages('tidyverse') # collection of R packages for data manipulation, analysis, and visualisation
## install.packages('lubridate') # working with dates and times
## remotes::install_github('eco4cast/neon4cast') # package from NEON4cast challenge organisers to assist with forecast building and submission

# ------ Load packages -----
library(tidyverse)
library(lubridate)
#--------------------------#

# Change this for your model ID
# Include the word "example" in my_model_id for a test submission
# Don't include the word "example" in my_model_id for a forecast that you have registered (see neon4cast.org for the registration form)
my_model_id <- 'example_ID'

# --Model description--- #

# Add a brief description of your modeling approach

# -- Uncertainty representation -- #

# Describe what sources of uncertainty are included in your forecast and how you estimate each source.

#------- Read data --------
# read in the targets data
targets <- read_csv("https://sdsc.osn.xsede.org/bio230014-bucket01/challenges/targets/project_id=neon4cast/duration=P1D/aquatics-targets.csv.gz")

# read in the sites data
aquatic_sites <- read_csv("https://raw.githubusercontent.com/eco4cast/neon4cast-ci/refs/heads/main/neon4cast_field_site_metadata.csv") |>
  dplyr::filter(aquatics == 1)

focal_sites <- "BARC"

# Filter the targets
targets <- targets %>%
  filter(site_id %in% focal_sites,
         variable == 'temperature')
#--------------------------#



# ------ Weather data ------
met_variables <- c("air_temperature")

# Past stacked weather -----
weather_past_s3 <- neon4cast::noaa_stage3()

weather_past <- weather_past_s3  |> 
  dplyr::filter(site_id %in% focal_sites,
                datetime >= ymd('2017-01-01'),
                variable %in% met_variables) |> 
  dplyr::collect()

# aggregate the past to mean values
weather_past_daily <- weather_past |> 
  mutate(datetime = as_date(datetime)) |> 
  group_by(datetime, site_id, variable) |> 
  summarize(prediction = mean(prediction, na.rm = TRUE), .groups = "drop") |> 
  # convert air temperature to Celsius if it is included in the weather data
  mutate(prediction = ifelse(variable == "air_temperature", prediction - 273.15, prediction)) |> 
  pivot_wider(names_from = variable, values_from = prediction)

#Add air temperature lags and average them
weather_past_daily<- weather_past_daily |> 
  group_by(site_id) |> 
  mutate(at_lag1 = lag(air_temperature, n=1), 
         at_lag2 = lag(air_temperature, n=2), 
         at_lag3 = lag(air_temperature, n=3), 
         p3_avg = (at_lag1 + at_lag2 + at_lag3)/3)

# Future weather forecast --------
# New forecast only available at 5am UTC the next day
forecast_date <- as.Date("2024-01-01")
noaa_date <- forecast_date - days(1)
ref_dates <- seq(forecast_date, as.Date("2024-12-31"), by= "1 month")
weather_future_2024 <- data.frame(datetime = as.Date(numeric()), site_id = character(), air_temperature = numeric(), parameter = integer())

for(d in 1:length(ref_dates)) {
  print(d)
  weather_future_s3 <- neon4cast::noaa_stage2(start_date = as.character(ref_dates[d]))
  
  weather_future <- weather_future_s3 |> 
    dplyr::filter(datetime >= forecast_date,
                  site_id %in% focal_sites,
                  variable %in% met_variables) |> 
    collect()
  
  weather_future_daily <- weather_future |> 
    mutate(datetime = as_date(datetime)) |> 
    # mean daily forecasts at each site per ensemble
    group_by(datetime, site_id, parameter, variable) |> 
    summarize(prediction = mean(prediction, na.rm = TRUE), .groups = "drop") |> 
    # convert air temperature to Celsius if it is included in the weather data
    mutate(prediction = ifelse(variable == "air_temperature", prediction - 273.15, prediction)) |> 
    pivot_wider(names_from = variable, values_from = prediction) |> 
    select(any_of(c('datetime', 'site_id', met_variables, 'parameter')))
  
  weather_future_2024 <- rbind(weather_future_2024, weather_future_daily)
}

weather_future_daily_2024 <- distinct(weather_future_2024)

#--------------------------#

n_members <- 30
# ----- Fit model & generate forecast----

# Generate a dataframe to fit the model to 
targets_lm <- targets |> 
  pivot_wider(names_from = 'variable', values_from = 'observation') |> 
  left_join(weather_past_daily, 
            by = c("datetime","site_id"))

# Loop through each site to fit the model
forecast_df <- NULL

for(i in 1:length(focal_sites)) {  
  
  curr_site <- focal_sites[i]
  
  for(d in 1:length(ref_dates)){
    stage_3_trim <- targets_lm |> 
      filter(datetime < ref_dates[d])
    
    site_target <- targets_lm |>
      filter(site_id == curr_site)
    
    fit <- lm(stage_3_trim$temperature ~ stage_3_trim$air_temperature)
    datetimes = seq(ref_dates[d], ref_dates[d] %m+% months(1))
    weather_future_curr <- weather_future_daily_2024 |> filter(datetime %in% datetimes)
    
    for(t in 1:length(datetimes)){
      for (ens in 1:n_members){
        
        temp_driv <- weather_future_curr |> 
          filter(datetime == datetimes[t],
                 parameter == ens)

        
        forecasted_temperature <- fit$coefficients[1] + fit$coefficients[2] * temp_driv$air_temperature
        curr_site_df <- tibble(datetime = temp_driv$datetime,
                               reference_datetime = ref_dates[d],
                               site_id = curr_site,
                               parameter = temp_driv$parameter,
                               prediction = forecasted_temperature,
                               variable = "temperature") #Change this if you are forecasting a different variable
        
        forecast_df <- dplyr::bind_rows(forecast_df, curr_site_df)
        print(paste(ref_dates[d], "-", datetimes[t], "-", ens, temp_driv$air_temperature, forecasted_temperature))
      }
    }
  }
  message(curr_site, ' forecast run')
}


#---- Covert to EFI standard ----

# Make forecast fit the EFI standards and plot
forecast_df_EFI <- forecast_df %>%
  filter(datetime > forecast_date) %>%
  mutate(model_id = my_model_id,
         reference_datetime = reference_datetime,
         family = 'ensemble',
         duration = 'P1D',
         parameter = as.character(parameter),
         project_id = 'neon4cast') %>%
  select(datetime, reference_datetime, duration, site_id, family, parameter, variable, prediction, model_id, project_id)

forecast_df_EFI |> 
  ggplot(aes(x=datetime, y=prediction, group = parameter)) +
  geom_line() +
  facet_wrap(~site_id) +
  labs(title = paste0('Forecast generated for ', forecast_df_EFI$variable[1], ' on ', forecast_df_EFI$reference_datetime[1]))

#--------------------------#
#Evaluate 
library(scoringRules)
targets_2024 <- targets_lm |> 
  filter(datetime <= as_datetime("2024-12-31"),
         datetime >= as_datetime("2024-01-01"))

forecasted_data <- forecast_df_EFI |> 
  filter(datetime <= as_datetime("2024-12-31"),
         datetime >= as_datetime("2024-01-01")) |> 
  mutate(horizon = as.numeric(datetime - reference_datetime)) |> 
  group_by(datetime, horizon) |> 
  summarize(mean = mean(prediction)) |> 
  filter(horizon != 31)

ref_dates = seq(as.Date("2024-01-01"), as.Date("2024-12-31"), by="day")

model_crps <- data.frame(horizon = integer(), crps = numeric(), datetime = as.Date(numeric()))  
for (t in 2:length(ref_dates)){
  targets_ref <- targets_2024 |> 
    filter(datetime == ref_dates[t])
  model_ref <- forecasted_data |> 
    filter(datetime == ref_dates[t])
  horizon <- model_ref$horizon
  crps <- crps_sample(targets_ref$temperature, model_ref$mean)
  model_crps<- model_crps |> add_row(horizon = horizon, crps = crps, datetime = ref_dates[t])
  print(ref_dates[t])
}
dummy <- cbind(horizon = c(31,32,33,34), mean_crps = as.numeric(c(NA,NA,NA,NA)), model_id = c("my_model","my_model","my_model","my_model"))
model_crps_avg <- model_crps |> 
  group_by(horizon) |> 
  summarize(mean_crps = as.numeric(mean(crps, na.rm = TRUE))) |> 
  mutate(model_id = "my_model") |>
  subset(!(horizon == 0))

baseline_models <- arrow::open_dataset("s3://anonymous@bio230014-bucket01/challenges/scores/bundled-parquet/project_id=neon4cast/duration=P1D/variable=temperature?endpoint_override=sdsc.osn.xsede.org") |> 
  filter(site_id == "BARC",
         reference_datetime < as_datetime("2024-12-31"),
         reference_datetime > as_datetime("2024-01-01"),
         model_id %in% c("climatology", "persistenceRW")) |> 
  collect()

crps_bl <- baseline_models |> 
  mutate(horizon = as.numeric(datetime - reference_datetime)) |> 
  summarize(mean_crps = mean(crps, na.rm = TRUE), .by = c("model_id", "horizon")) |> 
  pivot_wider(names_from = model_id, values_from = mean_crps) |> 
  subset(!(horizon %in% c(31, 32, 33, 34, 35))) |> 
  mutate(my_mod = model_crps_avg$mean_crps) |> 
  pivot_longer(cols = 2:4, names_to = "model", values_to = "crps")

ggplot(crps_bl, aes(y=crps, x=horizon, color = model)) + geom_line()
#--------------------------#



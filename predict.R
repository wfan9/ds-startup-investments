# Generate startup status prediction and confusion matrix.
# Code taken from report.RMD to satisfy project submission requirements of separate script.

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(stringi)) install.packages("stringi", repos = "http://cran.us.r-project.org")

####################
#
# Download Data
#
####################

dl <- tempfile()
download.file("https://github.com/wfan9/ds-startup-investments/raw/master/data/517018_952128_compressed_investments_VC.csv.zip", dl)
data <- read_csv(unzip(dl))

rm(dl)

####################
#
# Data Cleaning
#
####################

# Bad row with lots of NA.
data <- data %>% filter(!is.na(angel))

# Clean and transform status
data <- data %>% filter(!is.na(status)) %>%
  mutate(status = factor(status, levels = c("operating", "acquired", "closed")))

# permalink's of rows that look like bad data from manual inspection.
bad_permalink <- c("/organization/adometry-2", "/organization/ayoxxa-biosystems-2",
"/organization/busportal-2", "/organization/cgtrader-2", "/organization/cue-4", "/organization/gain-fitness",
"/organization/checkpoints", "/organization/lightex-ltd-", "/organization/realync-2",
"/organization/trippeace", "/organization/university-of-rochester-2", "/organization/victiv")
data <- data %>% filter(!(permalink %in% bad_permalink))

# Special case where the permalink is also duplicated, the only way to distinguish between the
# two is based on another field, such as market.
data <- data %>% filter(!(permalink == "/organization/treasure-valley-urology-services" & is.na(market)))

# Check for duplicate permalink values not removed during above process.
data %>% group_by(permalink) %>% summarise(n = n()) %>% filter(n > 1)

# After manually examining the output, use the following to remove the bad row:
data <- data %>% filter(!(permalink == "/organization/prysm" & is.na(market)))

# Clean market and categories.
data <- data %>% mutate(market = str_replace_na(market, "Unknown")) %>% mutate(market = factor(market))
all_markets <- unique(data$market)

data <- data %>% mutate(category_list = str_replace_na(category_list, "|Unknown|"))

# Drop the derived vars
data <- data %>% select(-founded_year, -founded_quarter, -founded_month)

# Remove companies too old to be considered startups.
data <- data %>% filter(founded_at >= as_date("1990-01-01"))

# Manually fix bad dates.
data[which(data$permalink == "/organization/fitfrnd-2"), "first_funding_at"] <- as_date("2014-11-26")
data[which(data$permalink == "/organization/fitfrnd-2"), "last_funding_at"] <- as_date("2014-11-26")
data[which(data$permalink == "/organization/agflow"), "first_funding_at"] <- as_date("2013-06-01")
data[which(data$permalink == "/organization/buru-buru"), "first_funding_at"] <- as_date("2013-04-01")
data[which(data$permalink == "/organization/exploco"), "first_funding_at"] <- as_date("2014-10-01")
data[which(data$permalink == "/organization/exploco"), "last_funding_at"] <- as_date("2014-10-01")
data[which(data$permalink == "/organization/nubank"), "first_funding_at"] <- as_date("2013-05-07")
data[which(data$permalink == "/organization/peoplegoal"), "first_funding_at"] <- as_date("2014-05-01")
data[which(data$permalink == "/organization/peoplegoal"), "last_funding_at"] <- as_date("2014-05-01")
data[which(data$permalink == "/organization/rotor"), "first_funding_at"] <- as_date("2014-09-29")
data[which(data$permalink == "/organization/rotor"), "last_funding_at"] <- as_date("2014-09-29")
data[which(data$permalink == "/organization/securenet-payment-systems"), "first_funding_at"] <- as_date("1997-01-01")
data[which(data$permalink == "/organization/shopboostr"), "first_funding_at"] <- as_date("2014-11-01")
data[which(data$permalink == "/organization/shopboostr"), "last_funding_at"] <- as_date("2014-11-01")
data[which(data$permalink == "/organization/the-urban-roosters"), "first_funding_at"] <- as_date("2014-07-01")
data[which(data$permalink == "/organization/the-urban-roosters"), "last_funding_at"] <- as_date("2014-07-01")

# Fix values for founded_at
data <- data %>% mutate(founded_at = if_else(is.na(founded_at), first_funding_at, founded_at))

# Manually set the values for funding dates.
ind <- which(data$name == "Jumper Networks")
data[ind, c("first_funding_at", "last_funding_at")] <- data$founded_at[ind]
ind <- which(data$name == "AndrewBurnett.com Ltd")
data[ind, c("first_funding_at", "last_funding_at")] <- data$founded_at[ind]

# Set founded equal to first funded at if necessary.
data <- data %>% mutate(founded_at = if_else(first_funding_at < founded_at, first_funding_at, founded_at))

# Change dates to just be year.
data <- data %>% mutate_at(c("founded_at", "first_funding_at", "last_funding_at"), year)

# Fix location variables.
data <- data %>% replace_na(list(country_code = "ZZZ", region = "Unknown", city = "Unknown")) %>%
  select(-state_code) %>%
  mutate_at(vars(country_code, region, city), as.factor)

# Clean funding_total_usd
data <- data %>% filter(funding_total_usd != "-")
data <- data %>% mutate(funding_total_usd = str_remove_all(funding_total_usd, ",")) %>%
  mutate(funding_total_usd = as.numeric(funding_total_usd))

# Drop home page.
data <- data %>% select(-homepage_url)


####################
#
# Feature Engineering
#
####################

# Convert factors to numeric and scale everything to [0,1] range to be compatible.
model_data <- data %>%
  mutate(country_code = as.numeric(country_code)[country_code]) %>%
  mutate(region = as.numeric(region)[region]) %>%
  mutate(funding = log10(funding_total_usd)) %>%
  mutate(rounds = pmax(funding_rounds, 5)) %>%
  mutate(market = as.numeric(market)[market]) %>%
  select(status, country_code, region, market, funding, rounds, founded_at, first_funding_at, last_funding_at) %>%
  mutate_at(2:9, scales::rescale) %>% as.data.frame()

formula <- status ~ country_code + region + market + funding + rounds + founded_at + first_funding_at + last_funding_at

# Split into training, validation and test data.
set.seed(1, sample.kind="Rounding")
index <- createDataPartition(y = model_data$status, times = 1, p = 0.3, list = FALSE)

training_set <- model_data[-index,]
remaining_set <- model_data[index,]

set.seed(1, sample.kind="Rounding")
index <- createDataPartition(y = remaining_set$status, times = 1, p = 0.5, list = FALSE)

validation_set <- remaining_set[-index,]
test_set <- remaining_set[index,]

# Generate balanced set.
if(!require(UBL)) install.packages("UBL", repos = "http://cran.us.r-project.org")
smoted_training_set <- UBL::SmoteClassif(formula, training_set)

# Train then compare against hold out test set.
smoted_multinom_model <- train(formula, method = "multinom", data = smoted_training_set) 

final_preds <- predict(smoted_multinom_model, test_set)
final_cm <- confusionMatrix(final_preds, test_set$status)
final_cm

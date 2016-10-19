accidents <- read_csv(file="accident.csv")

(acc_weekday_weekend <- select(accidents, MONTH, DAY, YEAR, DAY_WEEK))
(acc_weekday_weekend <- unite(acc_weekday_weekend, date, 1:3, sep="-"))
acc_weekday_weekend$date <- mdy(acc_weekday_weekend$date)
acc_weekday_weekend$date <- yday(acc_weekday_weekend$date)
acc_weekday_weekend <- mutate(acc_weekday_weekend,
                              tally = 1)
acc_weekday_weekend <- mutate(acc_weekday_weekend, 
                              weekend = (DAY_WEEK == 1) | (DAY_WEEK == 7))

acc_grouped <- acc_weekday_weekend%>%
  group_by(date) %>%
  summarise(tally=sum(tally), weekend = first(weekend))

ggplot(acc_grouped, aes(x=date, y = tally,color = weekend)) +
  geom_point()

state_fips <- 
  read.table("http://www2.census.gov/geo/docs/reference/state.txt", 
             header = TRUE,sep = "|")
state_fips <- select(state_fips, STATE, STATE_NAME)
state_fips <- rename(state_fips, state = STATE, state_name = STATE_NAME)

accident <- read_csv(file="accident.csv")
accident <- select(accident, STATE, DAY, MONTH, YEAR, HOUR, MINUTE, DRUNK_DR)
accident <- unite(accident, date, YEAR, MONTH, DAY, HOUR, MINUTE, 
                  sep = "-", remove = FALSE)
accident$date <- ymd_hm(accident$date)
accident <- rename(accident, state = STATE, drunk_dr = DRUNK_DR, month = MONTH)
accident <- select(accident, state, date, drunk_dr, month)
accident$drunk_dr <- as.logical(accident$drunk_dr)

accident <- mutate(accident, daytime = hour(accident$date) %in% 7:19)
st_name_accidents <- inner_join(accident, state_fips)

(colo_box <- subset(st_name_accidents, state_name == "Colorado"))
colo_box <- colo_box %>%
  group_by(month, daytime, drunk_dr) %>%
  summarize(fatalities = n())

ggplot(colo_box, aes(x = daytime, y = fatalities)) +
  geom_boxplot() +
  facet_grid(. ~ drunk_dr, labeller = label_both)


make_state_plot <- function(datafr, which_state) {
  if(!("state_name" %in% colnames(datafr))) {
    stop("Dataframe must have a column called 'state_name'")
  }
  if(!(which_state %in% unique(datafr$state_name))) {
    stop("state name ", which_state," not found")
  }
  state_box <- subset(datafr, state_name == which_state)
  state_box <- state_box %>%
    group_by(month, daytime, drunk_dr) %>%
    summarize(fatalities = n())
  ggplot(state_box, aes(x = daytime, y = fatalities)) +
    geom_boxplot() +
    facet_grid(. ~ drunk_dr, labeller = label_both) +
    ggtitle(paste("Fatal Accidents in", which_state))
}

pdf("stateplots.pdf")
for (i in c("Colorado", "Texas", "California", "New York")) {
  print(state_plot(st_name_accidents,i))
}
dev.off()

first_bits <- function(datafr, letter) {
  letter <- tolower(letter)
  list <- grep(paste0("^",letter), 
               unique(tolower(datafr$state_name)), value = TRUE)
  list <- str_to_title(list)
  return(list)
}

accident_map <- read_csv(file="accident.csv")
accident_map <- select(accident_map, LATITUDE, LONGITUD, STATE, DRUNK_DR)
accident_map <- dplyr::rename(accident_map, lat = LATITUDE, long = LONGITUD,
                       state = STATE, drunk_dr = DRUNK_DR)
accident_map$drunk_dr <- as.logical(accident_map$drunk_dr)
st_accident_map <- inner_join(accident_map, state_fips)
(st_accident_map <- select(st_accident_map, -state))

state_summaries <- st_accident_map %>%
  group_by(state_name) %>%
  dplyr::summarize(num = n(), percent_drunk = sum(drunk_dr)/n())

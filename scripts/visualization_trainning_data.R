########################################################################################################################
## Goal: Visualize trainnnig data
## Data source:  https://flow.polar.com/progress/running-index
########################################################################################################################

## Load libraries ####
pacman::p_load(here, 
               data.table, 
               dplyr, 
               ggplot2, 
               anytime, 
               patchwork, 
               hrbrthemes,
               ggtext)

theme_set(theme_ipsum_rc())
theme_set(theme_ipsum())
theme_set(theme_ft_rc())

## Configuration data ####
competition_list <- c("2019-12-31", "2019-12-29", "2020-09-20") # days where I run a competition

## Load data ####

polar_file <- max(list.files(path = (here("data")), pattern = "_polar_logs.txt", full.names = TRUE))

run_dt <- fread(polar_file, col.names = c("date", "activity", "running_index", "duration", "distance_km", "fc_med_ppm"))


## Prepare data ####
run_dt[, duration2 := hms(duration)] 

run_dt[, duration_min := hour(duration2)*60 + minute(duration2)] 

run_dt[, duration := NULL]
run_dt[, duration2 := NULL]

## Adjustments needed as I am collecting the data via web and localized in Spanish
run_dt[, new_date := sub(x = date, pattern = "^[a-zéá]{3}., ", replacement = "")]
run_dt[, new_date := sub(x = new_date, pattern = "dic", replacement = "dec")]
run_dt[, new_date := sub(x = new_date, pattern = "ene", replacement = "jan")]
run_dt[, new_date := sub(x = new_date, pattern = "abr", replacement = "apr")]
run_dt[, new_date := sub(x = new_date, pattern = "ago", replacement = "aug")]

run_dt[, date := anytime::anydate(new_date)] # arrange date format

run_dt[, new_date := NULL] # clean up

run_dt[, competition := fifelse(date %in% as.Date(competition_list), TRUE, FALSE)] # flag competition dates



## Visualization ####

p1 <- run_dt %>% 
  ggplot(aes(x = date, y = duration_min)) +
  geom_point() +
  geom_point(data = run_dt[competition == TRUE], color = "yellow") +
  geom_smooth(formula = y ~ x, method = 'loess') +
  labs(title = "Training duration [min] trend",
       x = NULL) 

p2 <- run_dt %>% 
  ggplot(aes(x = date, y = distance_km)) +
  geom_point() +
  geom_point(data = run_dt[competition == TRUE], color = "yellow") +
  geom_smooth(formula = y ~ x, method = 'loess') +
  labs(title = "Running distance [km] trend",
       x = NULL)

p3 <- run_dt %>% 
  ggplot(aes(x = date, y = duration_min/distance_km)) +
  geom_point() +
  geom_point(data = run_dt[competition == TRUE], color = "yellow") +
  scale_y_reverse() +
  geom_smooth(formula = y ~ x, method = 'loess') +
  labs(title = "Training speed trend",
       x = NULL)


p4 <- run_dt %>% 
  ggplot(aes(x = date, y = running_index)) +
  geom_point() +
  geom_point(data = run_dt[competition == TRUE], color = "yellow") +
  geom_smooth(formula = y ~ x, method = 'loess') +
  labs(title = "Running index trend",
       x = NULL)


p5 <- run_dt %>% 
  ggplot(aes(x = date, y = fc_med_ppm)) +
  geom_point() +
  geom_point(data = run_dt[competition == TRUE], color = "yellow") +
  scale_y_reverse() +
  geom_smooth(formula = y ~ x, method = 'loess') +
  labs(title = "FC med [ppm] trend",
       x = NULL)


p6 <- run_dt[, .(count = .N), .(week = floor_date(date, "weeks", week_start = 1))] %>% 
  ggplot(aes(x = week, y = count)) +
  geom_col() +
  scale_x_date() +
  labs(title = "Weekly runs",
       x = NULL)



(p1 + p2) / (p3 + p4) / (p5 + p6) + plot_annotation(
  title = "Javier's training and competition KPIs",
  subtitle = "Competitions in <b style='color:yellow'>yelllow</b>",
  caption = paste0("Day of Analysis: ", format(Sys.Date(), "%d %b %Y")),
  theme = theme(plot.title = element_markdown(lineheight = 1.1),
                plot.subtitle = element_markdown(lineheight = 1.1))
)
  

## Save data ####

ggsave(here("output",
            paste0(format(Sys.time(), "%Y%m%d_%H%M"),
                   "_running_kpis.png")), height = 12, width = 12)

g6 <- read_dta("~/Dropbox/data/gss_data/gsspanels/gsspanel06.dta")
g8 <- read_dta("~/Dropbox/data/gss_data/gsspanels/gsspanel08.dta") 
g10 <- read_dta("~/Dropbox/data/gss_data/gsspanels/gsspanel10.dta")


d6 <- g6 %>% select(id_1, dateintv_1, dateintv_2, dateintv_3) %>% mutate(ds = "06")
d8 <- g8 %>% select(id_1, dateintv_1, dateintv_2, dateintv_3) %>% mutate(ds = "08")
d10 <- g10 %>% select(id_1, dateintv_1, dateintv_2, dateintv_3) %>% mutate(ds = "10")

dates <- bind_rows(d6) %>%
  mutate(dateintv_1 = str_replace(dateintv_1,"(\\d{1})(\\d{2})$","\\1-\\2"),
         dateintv_2 = str_replace(dateintv_2,"(\\d{1})(\\d{2})$","\\1-\\2"),
         dateintv_3 = str_replace(dateintv_3,"(\\d{1})(\\d{2})$","\\1-\\2")) %>%
  mutate(y1 = recode(ds, "06"="2006", "08"="2008", "10"="2010"),
         y2 = recode(ds, "06"="2008", "08"="2010", "10"="2012"),
         y3 = recode(ds, "06"="2010", "08"="2012", "10"="2014")) %>%
  mutate(dateintv_1 = as.Date(paste("0", dateintv_1, "-", y1, sep = ""), format = "%m-%d-%Y"),
         dateintv_2 = as.Date(paste("0", dateintv_2, "-", y2, sep = ""), format = "%m-%d-%Y"),
         dateintv_3 = as.Date(paste("0", dateintv_3, "-", y3, sep = ""), format = "%m-%d-%Y")) %>%
  mutate(time21 = as.numeric(dateintv_2 - dateintv_1),
         time32 = as.numeric(dateintv_3 - dateintv_2))
  

dates %>% group_by(ds) %>%
  summarise(mean.time21 = mean(time21, na.rm = TRUE),
            mean.time32 = mean(time32, na.rm = TRUE))


dates %>%
  ggplot(aes(x = time21)) + 
  geom_histogram(color = "black", fill = "gray")

m1 <- lm(time32 ~ time21, data = dates)




g6 <- read_dta("~/Dropbox/data/gss_data/gsspanels/gsspanel06.dta")
g8 <- read_dta("~/Dropbox/data/gss_data/gsspanels/gsspanel08.dta") 
g10 <- read_dta("~/Dropbox/data/gss_data/gsspanels/gsspanel10.dta")

g6cs <- read_dta("~/Dropbox/data/gss_data/GSS2006.dta") %>%
  select(id, wtssall)
g8cs <- read_dta("~/Dropbox/data/gss_data/GSS2008.dta") %>%
  select(id, wtssall)
g10cs <- read_dta("~/Dropbox/data/gss_data/GSS2010.dta") %>%
  select(id, wtssall)

t <- g6 %>%
  select(id_1, trust_1, trust_2, trust_3, 
         helpful_1, helpful_2, helpful_3,
         fair_1, fair_2, fair_3, 
         health_1, health_2, health_3,
         tvhours_1, tvhours_2, tvhours_3, 
         socfrend_1, socfrend_2, socfrend_3, 
         socommun_1, socommun_2, socommun_3,
         socrel_1, socrel_2, socrel_3,
         socbar_1, socbar_2, socbar_3,
         attend_1, attend_2, attend_3,
         tvhours_1, tvhours_2, tvhours_3,
         educ_1, educ_2, educ_3,
         degree_1, degree_2, degree_3,
         marital_1, marital_2, marital_3,
         wrkstat_1, wrkstat_2, wrkstat_3,
         childs_1, childs_2, childs_3) %>%
  zap_labels() %>%
  mutate(trust_1 = recode(trust_1, "1"=1, "2"=0, "3"=.5),
         trust_2 = recode(trust_2, "1"=1, "2"=0, "3"=.5),
         trust_3 = recode(trust_3, "1"=1, "2"=0, "3"=.5),
         fair_1 = recode(fair_1, "2"=1, "1"=0, "3"=.5),
         fair_2 = recode(fair_2, "2"=1, "1"=0, "3"=.5),
         fair_3 = recode(fair_3, "2"=1, "1"=0, "3"=.5),
         helpful_1 = recode(helpful_1, "1"=1, "2"=0, "3"=.5),
         helpful_2 = recode(helpful_2, "1"=1, "2"=0, "3"=.5),
         helpful_3 = recode(helpful_3, "1"=1, "2"=0, "3"=.5),
         gtrust_1 = (trust_1 + fair_1 + helpful_1)/3,
         gtrust_2 = (trust_2 + fair_2 + helpful_2)/3,
         gtrust_3 = (trust_3 + fair_3 + helpful_3)/3,
         health_1 = 5 - health_1,
         health_2 = 5 - health_2,
         socfrend_1 = recode(socfrend_1, "1"=300, "2"=208, "3"=48,
                            "4"=12, "5"=4, "6"=1, "7"=0),
         socfrend_1 = sqrt(socfrend_1),
         socfrend_2 = recode(socfrend_2, "1"=300, "2"=208, "3"=48,
                            "4"=12, "5"=4, "6"=1, "7"=0),
         socfrend_2 = sqrt(socfrend_2),
         socfrend_3 = recode(socfrend_3, "1"=300, "2"=208, "3"=48,
                             "4"=12, "5"=4, "6"=1, "7"=0),
         socfrend_3 = sqrt(socfrend_3),
         socommun_1 = recode(socommun_1, "1"=300, "2"=208, "3"=48,
                            "4"=12, "5"=4, "6"=1, "7"=0),
         socommun_1 = sqrt(socommun_1),
         socommun_2 = recode(socommun_2, "1"=300, "2"=208, "3"=48,
                            "4"=12, "5"=4, "6"=1, "7"=0),
         socommun_2 = sqrt(socommun_2),
         socommun_3 = recode(socommun_3, "1"=300, "2"=208, "3"=48,
                             "4"=12, "5"=4, "6"=1, "7"=0),
         socommun_3 = sqrt(socommun_3),
         socrel_1 = recode(socrel_1, "1"=300, "2"=208, "3"=48,
                            "4"=12, "5"=4, "6"=1, "7"=0),
         socrel_1 = sqrt(socrel_1),
         socrel_2 = recode(socrel_2, "1"=300, "2"=208, "3"=48,
                          "4"=12, "5"=4, "6"=1, "7"=0),
         socrel_2 = sqrt(socrel_2),
         socrel_3 = recode(socrel_3, "1"=300, "2"=208, "3"=48,
                           "4"=12, "5"=4, "6"=1, "7"=0),
         socrel_3 = sqrt(socrel_3),
         socbar_1 = recode(socbar_1, "1"=300, "2"=208, "3"=48,
                          "4"=12, "5"=4, "6"=1, "7"=0),
         socbar_1 = sqrt(socbar_1),
         socbar_2 = recode(socbar_2, "1"=300, "2"=208, "3"=48,
                          "4"=12, "5"=4, "6"=1, "7"=0),
         socbar_2 = sqrt(socbar_2),
         socbar_3 = recode(socbar_3, "1"=300, "2"=208, "3"=48,
                           "4"=12, "5"=4, "6"=1, "7"=0),
         socbar_3 = sqrt(socbar_3),
         
         socindex_1 = (socfrend_1 + socommun_1 + socrel_1 + socbar_1)/4,
         socindex_2 = (socfrend_2 + socommun_2 + socrel_2 + socbar_2)/4,
         socindex_3 = (socfrend_3 + socommun_3 + socrel_3 + socbar_3)/4,
         attend_1 = recode(attend_1, "1"=0, "1"=.5, "2"=1, "3"=4, "4"=12, "5"=30,
                          "6"=42, "7"=52, "8"=100),
         #attend_1 = sqrt(attend_1),
         attend_2 = recode(attend_2, "1"=0, "1"=.5, "2"=1, "3"=4, "4"=12, "5"=30,
                          "6"=42, "7"=52, "8"=100),
         #attend_2 = sqrt(attend_2),
         attend_3 = recode(attend_3, "1"=0, "1"=.5, "2"=1, "3"=4, "4"=12, "5"=30,
                           "6"=42, "7"=52, "8"=100),
         #attend_3 = sqrt(attend_3),
         degree_1 = degree_1,
         degree_2 = degree_2,
         degree_3 = degree_3,
         divorced_1 = ifelse(marital_1 == 3,1,0),
         divorced_2 = ifelse(marital_2 == 3,1,0),
         divorced_3 = ifelse(marital_3 == 3,1,0),
         haschild_1 = ifelse(childs_1 > 0, 1, 0),
         haschild_2 = ifelse(childs_2 > 0, 1, 0),
         haschild_3 = ifelse(childs_3 > 0, 1, 0),
         unemploy_1 = ifelse(is.na(wrkstat_1), NA,ifelse(wrkstat_1 %in% c(3,4), 1, 0)),
         unemploy_2 = ifelse(is.na(wrkstat_2), NA,ifelse(wrkstat_2 %in% c(3,4), 1, 0)),
         unemploy_3 = ifelse(is.na(wrkstat_3), NA,ifelse(wrkstat_3 %in% c(3,4), 1, 0))) %>%
  left_join(g6cs, by=c("id_1"="id")) %>%
  left_join(dates)




t.wide <- t %>% 
  mutate(divorced = ifelse(divorced_1 == 0 & divorced_3 == 1, 1, 0),
         degree = ifelse(degree_3 > degree_1, 1, 0),
         haschild = ifelse(haschild_1 == 0 & haschild_3 == 1, 1, 0),
         unemploy = ifelse(unemploy_1 == 0 & unemploy_3 == 1, 1, 0))

t.wide2 <- t.wide %>%
  mutate(deltatrust = gtrust_3 - gtrust_1,
         #deltatrust2 = gtrust_3 - gtrust_2,
         deltaties = socindex_3 - socindex_1,
         #deltaties2 = socindex_3 - socindex_2,
         deltahealth = health_3 - health_1,
         #deltahealth2 = health_3 - health_2,
         deltatv = tvhours_3 - tvhours_1,
         #deltatv2 = tvhours_3 - tvhours_2,
         deltaattend = attend_3 - attend_1
         #deltaattend2 = attend_3 - attend_2
         ) %>%
  select(id_1, wtssall, divorced, degree, haschild, unemploy, deltaties, 
         deltahealth, deltaattend, deltatv, deltatrust, dateintv_1, dateintv_2) %>%
  mutate(miss = ifelse(is.na(deltatrust), 1, 0)) 

ggplot(t.wide2 %>% filter(!is.na(deltatrust)), 
       aes(x = dateintv_1, y = dateintv_2, fill = deltatrust)) + 
  geom_point(shape = 21) + 
  scale_fill_viridis_b()


ggplot(t.wide2, aes(x = deltaties, y = deltatrust)) + 
  geom_jitter() +
  geom_smooth(method = "loess") + 
  theme_minimal()

mice2 <- mice(t.wide2, m = 30)

fit.all <- with(mice2, lm(deltatrust ~ divorced + degree + haschild + unemploy + 
                 deltaties + deltahealth + deltaattend + deltatv,
                 weights = wtssall))

g6_13_fd <- summary(pool(fit.all))
g6_12_fd <- g6_23_fd
g6_23_fd <- summary(pool(fit.all))

df.1 <- complete(mice2, 1)

cor(df.1$deltatrust, df.1$deltatrust2)

#Main replication
g6_results <- summary(pool(fit.all)) %>% mutate(waves = "06-1-2")
g8_results <- summary(pool(fit.all)) %>% mutate(waves = "08-1-2")
g10_results <- summary(pool(fit.all)) %>% mutate(waves = "10-1-2")

g10_results_23 <- summary(pool(fit.all)) %>% mutate(waves = "10-2-3")
g8_results_23 <- summary(pool(fit.all)) %>% mutate(waves = "08-2-3")
g6_results_23 <- summary(pool(fit.all)) %>% mutate(waves = "06-2-3")


bind_rows(g6_results, g6_results_23, g8_results, g8_results_23,
          g10_results, g10_results_23) %>%
  filter(term == "divorced") %>%
  mutate(waves = ifelse(is.na(waves), "06-1-2", waves)) %>%
  ggplot(aes(x = waves, y = estimate)) + 
  geom_hline(yintercept = 0, linetype = 21) + 
  geom_linerange(aes(ymin = estimate - 1.96 * std.error,
                     ymax = estimate + 1.96 * std.error)) + 
  geom_point(shape = 21, fill = "gray") + 
  coord_flip() + 
  theme_minimal() + 
  labs(x = "", y = "Coefficient estimate")

paxton_model <- '
  
  #structural model
  gtrust_2 ~ 1*gtrust_1 + divorced + degree + haschild + unemploy + 
    delta_ties + delta_health + delta_tv + delta_attend
    
  delta_ties =~ socindex_2 + -1*socindex_1
  delta_health =~ health_2 + -1*health_1
  delta_tv =~ tvhours_2 + -1*tvhours_1
  delta_attend =~ attend_2 + -1*attend_1
  
  delta_ties ~~ 0*delta_health + 0*delta_tv + 0*delta_attend
  delta_health ~~ 0*delta_tv + 0*delta_attend
  delta_tv ~~ 0*delta_attend
  
  gtrust_1 ~~ 0*divorced + 0*degree + 0*haschild + 0*unemploy + 
    0*delta_ties + 0*delta_health + 0*delta_tv + 0*delta_attend 
'



m1 <- sem(paxton_model, data = t.wide2,
          missing = "ML", fixed.x = FALSE, 
          check.gradient = FALSE)
summary(m1)
semPaths(m1, intercepts = FALSE)




t.long <- t %>%
  select(-c(ds, y1, y2, y3, time21, time32)) %>%
  gather(key = "key", value = "value", -c(id_1, wtssall)) %>%
  separate(key, into=c("var", "wave"), sep = "_") %>%
   spread(var, value) %>%
  select(id_1, wtssall, wave, gtrust, divorced, degree, 
         haschild, unemploy, health, attend, tvhours, 
         socindex, socfrend, socommun, socrel, socbar) %>%
  mutate(miss = ifelse(is.na(gtrust), 1, 0)) %>%
  select(-c(socfrend, socommun, socrel, socbar))



mice1 <- mice(t.long)

df1 <- complete(mice1, action = 'long', include = TRUE) %>%
  #mutate(gtrust = ifelse(miss == 1, NA, 0)) %>%
  group_by(.imp, id_1) %>%
  mutate(across(c(gtrust, divorced, degree, haschild, unemploy,
                  health, attend, tvhours, socindex), ~ .x - mean(.x, na.rm = TRUE)))
  
fit <- with(as.mids(df1, where = NULL), lm(gtrust ~ divorced + degree + haschild + unemploy + health + 
                        attend + tvhours + socindex,
                        weights = wtssall))

g6_fe <- summary(pool(fit))

df1 %>% purrr::map(mutate(.x, miss = miss + 1))

m1 <- plm(gtrust ~ divorced + degree + haschild + unemploy + health + 
            attend + tvhours + socindex, index = c("id_1", "wave"),
          weights = wtssall, data = df1)
summary(m1)

%>%
  group_by(id_1) %>% 
  mutate(across(c(gtrust, divorced, degree, haschild, unemploy,
                health, attend, tvhours, socindex, socfrend,
                socommun, socrel, socbar), ~.x - lag(.x))) %>%
  mutate(divorced = ifelse(divorced < 0, 0, divorced),
         degree = ifelse(degree < 0, 0, ifelse(degree > 0, 1, 0)),
         haschild = ifelse(haschild < 0, 0, haschild),
         unemploy = ifelse(unemploy < 0, 0, unemploy))

fit.lm <- lm(gtrust ~ divorced + degree + haschild + unemploy + health + 
               attend + tvhours + socfrend, data = df1)
summary(fit.lm)

fit1 <- plm(gtrust ~ divorced + degree + haschild + unemploy + health + 
              attend + tvhours + socfrend,
            data = df1, index = c("id_1", "wave"))
summary(fit1)

with(mice1, plm(gtrust ~ divorced + degree + haschild + unemploy + 
                  health + attend + tvhours + socindex, index = c("id_1"),
                data = .))


m1 <- plm(gtrust ~ divorced + degree + haschild + unemploy + 
            health + attend + tvhours + socindex,
          index = c("id_1"), model = "within",
          data = t.long)


paxton_model <- "

  #Measurement Part
  deltahealth =~ 1*health_2 + -1*health_1
  deltaties =~ 1*socindex_2 + -1*socindex_1
  deltatnd =~ 1*attend_2 + -1*attend_1
  deltatv =~ 1*tvhours_2 + -1*tvhours_1

  #Structural Part
  gtrust_2 ~ 1*gtrust_1 + divorced + degree + unemploy + haschild + 
  deltahealth + deltaties + deltatnd + deltatv
  
  #Covariances
  divorced ~~ degree + unemploy + haschild + deltahealth + deltaties + deltatnd + deltatv
  degree ~~ unemploy + haschild + deltahealth + deltaties + deltatnd + deltatv
  unemploy ~~ haschild + deltahealth + deltaties + deltatnd + deltatv
  haschild ~~ deltahealth + deltaties + deltatnd + deltatv
  deltahealth ~~ deltaties + deltatnd + deltatv
  deltaties ~~ deltatnd + deltatv
  deltatnd ~~ deltatv

  gtrust_1 ~~ 0*divorced + 0*degree + 0*unemploy + 0*haschild +
  0*health_2 + 0*health_1 + 0*socindex_2 + 0*socindex_1 + 0*attend_2 + 
  0*attend_1 + 0*tvhours_2 + 0*tvhours_1
  
  deltahealth ~~ 0*gtrust_1
  deltaties ~~ 0*gtrust_1
  deltatnd ~~ 0*gtrust_1
  deltatv ~~ 0*gtrust_1
  
" 


summary(pool(fit1))



libary(mice)
library(mice)













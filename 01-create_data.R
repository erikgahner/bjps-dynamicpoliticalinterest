# Packages
library("psy")
library("tidyverse")
library("rio")

two_sd <- function(x) {
  return((x - mean(x, na.rm = TRUE))/(2*sd(x, na.rm = TRUE)))
}


data_bg <- import("~/Google Drev/data/liss/b/avars_200905_EN_2.0p.dta")
data_pers <- import("~/Google Drev/data/liss/pe/cp09b_1.0p_EN.dta")
data_pre <- import("~/Google Drev/data/liss/ec/ay09a_ENG_1.0p.dta")
data_post <- import("~/Google Drev/data/liss/ec/ay09b_ENG_1.0p.dta")
data_pol <- import("~/Google Drev/data/liss/po/cv10c_EN_1.0p.dta")

liss <- left_join(data_pre, data_post, by="nomem_encr")
liss <- left_join(liss, data_pers, by="nomem_encr")
liss <- left_join(liss, data_bg, by="nomem_encr")
liss <- left_join(liss, data_pol, by="nomem_encr")

liss <- liss %>%
  mutate(
    male = if_else(geslacht == 1, 1, 0),
    # Openness
    cp09b029rc = 6 - cp09b029,
    cp09b039rc = 6 - cp09b039,
    cp09b049rc = 6 - cp09b049,
    
    # Conscientiousness
    cp09b027rc = 6 - cp09b027,
    cp09b037rc = 6 - cp09b037,
    cp09b047rc = 6 - cp09b047,
    cp09b057rc = 6 - cp09b057,
    
    ### Extraversion
    cp09b025rc = 6 - cp09b025,
    cp09b035rc = 6 - cp09b035,
    cp09b055rc = 6 - cp09b055,
    cp09b065rc = 6 - cp09b065,
    cp09b045rc = 6 - cp09b045,
    
    ## Agreeableness
    cp09b021rc = 6 - cp09b021,
    cp09b031rc = 6 - cp09b031,
    cp09b051rc = 6 - cp09b051,
    cp09b041rc = 6 - cp09b041,
    
    ## Neuroticism
    cp09b038rc = 6 - cp09b038,
    cp09b028rc = 6 - cp09b028
    
  )

## Create personality traits
liss <- within(liss, O <- cp09b024 + cp09b034 + cp09b044 + cp09b054 + cp09b059 + cp09b064 + cp09b069 + cp09b029rc + cp09b039rc + cp09b049rc)
liss <- within(liss, C <- cp09b022 + cp09b027rc + cp09b032 + cp09b037rc + cp09b042 + cp09b047rc + cp09b052 + cp09b057rc + cp09b062 + cp09b067)
liss <- within(liss, E <- cp09b020 + cp09b025rc + cp09b030 + cp09b035rc + cp09b050 + cp09b055rc + cp09b060 + cp09b065rc + cp09b040 + cp09b045rc)
liss <- within(liss, A <- cp09b021rc + cp09b026 + cp09b066 + cp09b031rc + cp09b036 + cp09b061 + cp09b046 + cp09b051rc + cp09b041rc + cp09b056)
liss <- within(liss, N <- cp09b063 + cp09b038rc + cp09b043 + cp09b048 + cp09b023 + cp09b068 + cp09b028rc + cp09b033 + cp09b053 + cp09b058)

#### Get Cronbach's reliability coefficient alpha
with(liss, cronbach(data.frame(cp09b024, cp09b034, cp09b044, cp09b054, cp09b059, cp09b064, cp09b069, cp09b029rc, cp09b039rc, cp09b049rc)))$alpha
with(liss, cronbach(data.frame(cp09b022, cp09b027rc, cp09b032, cp09b037rc, cp09b042, cp09b047rc, cp09b052, cp09b057rc, cp09b062, cp09b067)))$alpha
with(liss, cronbach(data.frame(cp09b020, cp09b025rc, cp09b030, cp09b035rc, cp09b050, cp09b055rc, cp09b060, cp09b065rc, cp09b040, cp09b045rc)))$alpha
with(liss, cronbach(data.frame(cp09b021rc, cp09b026, cp09b066, cp09b031rc, cp09b036, cp09b061, cp09b046, cp09b051rc, cp09b041rc, cp09b056)))$alpha
with(liss, cronbach(data.frame(cp09b063, cp09b038rc, cp09b043, cp09b048, cp09b023, cp09b068, cp09b028rc, cp09b033, cp09b053, cp09b058)))$alpha

liss <- liss %>%
  # Recode personality traits
  mutate(
    O_sd = two_sd(O),
    C_sd = two_sd(C),
    E_sd = two_sd(E),
    A_sd = two_sd(A),
    N_sd = two_sd(N)
  ) %>%
  
  mutate(
    # Create political interest difference
    pintr_dif = ay09a018 - ay09b018,
    
    # Create ideology measure
    ideology = ifelse(cv10c101 == 999, NA, cv10c101)
    
  ) 


liss %>% 
  select(pintr_dif, O_sd, C_sd, E_sd, A_sd, N_sd, 
         male, leeftijd, oplzon, ideology,
         ay09a018, ay09b018,
         cp09b024, cp09b034, cp09b044, cp09b054, cp09b059, cp09b064, cp09b069, cp09b029rc, cp09b039rc, cp09b049rc) %>% 
  drop_na(pintr_dif, O_sd) %>% 
  write_csv("study1.csv")


selects <- import("~/Google Drev/data/selects/2015/828_Selects2015_PanelRCS_Data_v1.1.sav")

selects <- selects %>%
  # Recode missing
  mutate_at(vars(W3_f15770a, W3_f15770b, W3_f15770c, W3_f15770d, W3_f15770e, W3_f15770f, 
                 W3_f15770g, W3_f15770h, W3_f15771a, W3_f15771b, W3_f15771c, W3_f15771d, 
                 W3_f15771e, W3_f15771f, W3_f15771g), 
            function(x) case_when(x == 99 ~ NA_real_, TRUE ~ as.numeric(x))) %>%
  mutate(polint_pre = ifelse(W2_f10100 < 7, 5 - W2_f10100, NA),
         polint_post = ifelse(W3_f10100 < 7, 5 - W3_f10100, NA),
         pintr_dif = polint_pre - polint_post,
         
         male = ifelse(sex == 1, 1, 0),
         age = ifelse(age == 999, NA, age),
         education = ifelse(f21310 < 14, f21310, NA),
         ideology = f15201,
         
         O = W3_f15771a + W3_f15771f + W3_f15770d,
         C = W3_f15770a + (10-W3_f15770g) + W3_f15771c,
         E = W3_f15770b + W3_f15770h + (10-W3_f15771d),
         A = (10-W3_f15770c) + W3_f15770f + W3_f15771e,
         N = W3_f15770e + W3_f15771b + (10-W3_f15771g),
         O_sd = two_sd(O),
         C_sd = two_sd(C),
         E_sd = two_sd(E),
         A_sd = two_sd(A),
         N_sd = two_sd(N)
  ) 


#### Get Cronbach's reliability coefficient alpha
with(selects, cronbach(data.frame(W3_f15771a, W3_f15771f, W3_f15770d)))$alpha
with(selects, cronbach(data.frame(W3_f15770a, (10-W3_f15770g), W3_f15771c)))$alpha
with(selects, cronbach(data.frame(W3_f15770b, W3_f15770h, (10-W3_f15771d))))$alpha
with(selects, cronbach(data.frame((10-W3_f15770c), W3_f15770f, W3_f15771e)))$alpha
with(selects, cronbach(data.frame(W3_f15770e, W3_f15771b, (10-W3_f15771g))))$alpha

selects %>% 
  select(pintr_dif, polint_pre, polint_post, O_sd, C_sd, E_sd, A_sd, N_sd, male, age, education, ideology, W3_f15771a, W3_f15771f, W3_f15770d) %>% 
  write_csv("study2a.csv")


data_pers <- import("~/Google Drev/data/shp/W11_2009/shp09_p_user.dta")
data_election <- import("~/Google Drev/data/shp/W13_2011/shp11_p_user.dta")
data_placebo <- import("~/Google Drev/data/shp/W12_2010/shp10_p_user.dta")

shp <- left_join(data_election, data_pers, by="idpers")
shp <- left_join(shp, data_placebo, by="idpers")

shp <- shp %>%
  # Recode missing
  mutate_at(vars(p10p01, p11p01, p09c60:p09c69, age11), 
            function(x) case_when(x < 0 ~ NA_real_, TRUE ~ as.numeric(x))) %>%
  
  # Rename political interest
  rename(pintr_placebo = p10p01,
         pintr = p11p01,
         age = age11) %>%
  
  # Reverse code
  mutate(
    p09c60rc = (p09c60 - 10)*-1,
    p09c66rc = (p09c66 - 10)*-1,
    p09c67rc = (p09c67 - 10)*-1,
    p09c68rc = (p09c68 - 10)*-1
  ) %>%
  
  # Generate personality traits
  mutate(
    O = (p09c64 + p09c69)/20,
    C = (p09c62 + p09c67rc -1)/19,
    E = (p09c60rc + p09c65)/20,
    A = (p09c61 + p09c66rc)/20,
    N = (p09c63 + p09c68rc)/20
  ) %>%
  
  # Recode personality traits
  mutate(
    O_sd = two_sd(O),
    C_sd = two_sd(C),
    E_sd = two_sd(E),
    A_sd = two_sd(A),
    N_sd = two_sd(N)
  ) %>%
  
  # Create covariates
  mutate(
    male = ifelse(sex11 == 1, 1, 0),
    edu = ifelse(edu_1_09 < 0, NA, edu_1_09),
    ideology = ifelse(p09p10 < 0, NA, p09p10)
  ) %>%
  
  # Create date variables
  mutate(
    month10 = format(pdate10, "%m"),
    day10 = format(pdate10, "%d"),
    month11 = format(pdate11, "%m"),
    day11 = format(pdate11, "%d")
  ) %>%
  
  # Generate treatment variable
  mutate(
    election = case_when(month11 == 10 & day11 < 23 & day11 > 15 ~ 1, 
                         month11 == 10 & day11 > 23 & day11 < 31 ~ 0,
                         TRUE ~ NA_real_),
    
    placebo = case_when(month10 == 10 & day10 < 23 & day10 > 15 ~ 1, 
                        month10 == 10 & day10 > 23 & day10 < 31 ~ 0,
                        TRUE ~ NA_real_)
  )

#### Get Cronbach's reliability coefficient alpha
with(shp, cronbach(data.frame(p09c64, p09c69)))$alpha
with(shp, cronbach(data.frame(p09c62, p09c67rc)))$alpha
with(shp, cronbach(data.frame(p09c60rc, p09c65)))$alpha
with(shp, cronbach(data.frame(p09c61, p09c66rc)))$alpha
with(shp, cronbach(data.frame(p09c63, p09c68rc)))$alpha

shp$Campaign <- recode(shp$election,
                       `1` = "Yes",
                       `0` = "No")


shp %>% 
  select(election, placebo, pintr, pintr_placebo, male, age, edu, ideology, O_sd, C_sd, E_sd, A_sd, N_sd, p09c64, p09c69) %>% 
  write_csv("study2b.csv")



data_1996 <- import("~/Google Drev/data/bhps/stata8/findresp.dta")
data_2001 <- import("~/Google Drev/data/bhps/stata8/kindresp.dta")
data_2005 <- import("~/Google Drev/data/bhps/stata8/oindresp.dta")

bhps <- left_join(data_2001, data_1996, by="pid")
bhps <- left_join(bhps, data_2005, by="pid")

bhps <- bhps %>%
  # Recode missing
  mutate_at(vars(kvote6, fvote6, optrt5a1:optrt5o3), 
            function(x) case_when(x < 0 ~ NA_real_, TRUE ~ as.numeric(x))) %>%
  
  # Rename political interest
  mutate(pintr_placebo = (fvote6 - 4)*-1,
         pintr = (kvote6 - 4)*-1
  ) %>%
  
  # Generate personality traits
  mutate(
    O = optrt5o1 + optrt5o2 + optrt5o3,
    C = (8 - optrt5c2) + optrt5c1 + optrt5c3,
    E = (8 - optrt5e3) + optrt5e1 + optrt5e2,
    A = (8 - optrt5a1) + optrt5a2 + optrt5a3,
    N = (8 - optrt5n3) + optrt5n1 + optrt5n2
  ) %>%
  
  # Create covariates
  mutate(
    male = ifelse(osex == 1, 1, 0),
    age = ifelse(oage == -9, NA, oage),
    edu = ifelse(oqfedhi < 0, NA, oqfedhi),
    party = kvote3
  ) %>%
  
  # Generate treatment variable
  mutate(
    terror = case_when(kdoim == 9 & kdoid < 11 & kdoid > 3~ 0, 
                       kdoim == 9 & kdoid > 11 & kdoid < 19 ~ 1,
                       TRUE ~ NA_real_),
    
    placebo = case_when(fdoim == 9 & fdoid < 11 & fdoid > 3 ~ 0, 
                        fdoim == 9 & fdoid > 11 & fdoid < 19 ~ 1,
                        TRUE ~ NA_real_)
  ) %>%
  
  mutate(O_sd = two_sd(O),
         C_sd = two_sd(C),
         E_sd = two_sd(E),
         A_sd = two_sd(A),
         N_sd = two_sd(N)
  )

#### Get Cronbach's reliability coefficient alpha
with(bhps, cronbach(data.frame(optrt5o1, optrt5o2, optrt5o3)))$alpha
with(bhps, cronbach(data.frame((8 - optrt5c2), optrt5c1, optrt5c3)))$alpha
with(bhps, cronbach(data.frame((8 - optrt5e3), optrt5e1, optrt5e2)))$alpha
with(bhps, cronbach(data.frame((8 - optrt5a1), optrt5a2, optrt5a3)))$alpha
with(bhps, cronbach(data.frame((8 - optrt5n3), optrt5n1, optrt5n2)))$alpha

bhps %>% 
  select(terror, placebo, pintr, pintr_placebo, male, age, edu, party, O_sd, C_sd, E_sd, A_sd, N_sd, optrt5o1, optrt5o2, optrt5o3) %>% 
  write_csv("study3.csv")



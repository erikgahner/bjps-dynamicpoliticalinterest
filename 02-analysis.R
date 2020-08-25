library("interplot")
library("stargazer")
library("grid")
library("gridExtra")
library("scales")
library("broom")
library("xtable")
library("magrittr")
library("digest")
library("tidyverse")

liss <- read_csv("study1.csv")
selects <- read_csv("study2a.csv")
shp <- read_csv("study2b.csv")
bhps <- read_csv("study3.csv")

# Check whether data is correct
if (sha1(liss) != "b5eb2f69c7ea9f867d6e744da72ad49cf28e0a48"){ error("Wrong data file loaded or data has been changed!") }
if (sha1(selects) != "781abe8787b1c979e954dde175742f34030770d8"){ error("Wrong data file loaded or data has been changed!") }
if (sha1(shp) != "a240299b53de554524833a2f595f480f753a2c55"){ error("Wrong data file loaded or data has been changed!") }
if (sha1(bhps) != "0a709bb7443f40cd02783747273ee2382ed282b2"){ error("Wrong data file loaded or data has been changed!") }

two_sd <- function(x) {
  return((x - mean(x, na.rm = TRUE))/(2*sd(x, na.rm = TRUE)))
}

# Load theme
theme_set(
  theme_grey(base_size = 11.5) %+replace% 
    theme(panel.background = element_rect(fill = "gray98", colour = "gray98"),
          panel.grid.major.x = element_blank(), 
          panel.grid.major.y = element_line(colour = "gray90", size = 0.2),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = 12, hjust = 0, margin = margin(b = 15)),
          axis.title = element_text(colour = "gray40",  face = "plain"),
          axis.text = element_text(colour = "gray50", face = "plain"),
          axis.ticks = element_line(colour = "gray90")
    )
)

liss %>% 
  filter(!is.na(pintr_dif) & 
           !is.na(O_sd) & 
           !is.na(C_sd) &
           !is.na(E_sd) &
           !is.na(A_sd) &
           !is.na(N_sd)) %>%
  select(ay09a018, ay09b018, O_sd, C_sd, E_sd, A_sd, N_sd, male, leeftijd, ideology) %>%
  data.frame() %>% 
  stargazer(out="tabA2.htm",
            covariate.labels = c("Political interest, campaign", 
                                 "Political interest, non-campaign",
                                 "Openness",
                                 "Conscientiousness", "Extraversion", "Agreeableness",
                                 "Neuroticism", "Male", "Age", "Ideology"),
            median = TRUE, iqr = TRUE,
            title = "Summary statistics, LISS",
            digits = 2, type="text")


liss %>% 
  filter(!is.na(pintr_dif) & 
           !is.na(O_sd) & 
           !is.na(C_sd) &
           !is.na(E_sd) &
           !is.na(A_sd) &
           !is.na(N_sd)) %>%
  select(ay09a018, ay09b018, O_sd, C_sd, E_sd, A_sd, N_sd, male, leeftijd) %>%
  cor(., use="pairwise.complete.obs") %>%
  stargazer(., 
            covariate.labels = c("Political interest, campaign", 
                                 "Political interest, non-campaign",
                                 "Openness",
                                 "Conscientiousness", "Extraversion", "Agreeableness",
                                 "Neuroticism", "Male", "Age"),
            type="text", digits = 2, out="tabA3.htm")


reg.1 <- lm(pintr_dif ~ O_sd, data=liss)
reg.2 <- lm(pintr_dif ~ O_sd + C_sd + E_sd + A_sd + N_sd + male + leeftijd + factor(oplzon), data=liss)
reg.3 <- lm(pintr_dif ~ O_sd + C_sd + E_sd + A_sd + N_sd + male + leeftijd + factor(oplzon) + ideology, data=liss)

liss_facet_df <- data.frame(facet = c("cp09b024", "cp09b034", "cp09b044", "cp09b054", "cp09b059", "cp09b064", "cp09b069", "cp09b029rc", "cp09b039rc", "cp09b049rc"),
                            name = c("Rich vocabulary", "Vivid imagination", "Excellent ideas", "Quick to understand things",
                                     "Use difficult words", "Reflect on things", "Full of ideas", "Understand abstract ideas",
                                     "Interested in abstract ideas", "Good imagination"),
                            estimate = NA,
                            stderror = NA,
                            pvalue = NA)

liss_facet_df$estimate[liss_facet_df$facet == "cp09b024"] <- tidy(summary(lm(pintr_dif ~ cp09b024, data=liss)))[2, c("estimate")]
liss_facet_df$stderror[liss_facet_df$facet == "cp09b024"] <- tidy(summary(lm(pintr_dif ~ cp09b024, data=liss)))[2, c("std.error")]
liss_facet_df$pvalue[liss_facet_df$facet == "cp09b024"] <- tidy(summary(lm(pintr_dif ~ cp09b024, data=liss)))[2, c("p.value")]
liss_facet_df$estimate[liss_facet_df$facet == "cp09b034"] <- tidy(summary(lm(pintr_dif ~ cp09b034, data=liss)))[2, c("estimate")]
liss_facet_df$stderror[liss_facet_df$facet == "cp09b034"] <- tidy(summary(lm(pintr_dif ~ cp09b034, data=liss)))[2, c("std.error")]
liss_facet_df$pvalue[liss_facet_df$facet == "cp09b034"] <- tidy(summary(lm(pintr_dif ~ cp09b034, data=liss)))[2, c("p.value")]
liss_facet_df$estimate[liss_facet_df$facet == "cp09b044"] <- tidy(summary(lm(pintr_dif ~ cp09b044, data=liss)))[2, c("estimate")]
liss_facet_df$stderror[liss_facet_df$facet == "cp09b044"] <- tidy(summary(lm(pintr_dif ~ cp09b044, data=liss)))[2, c("std.error")]
liss_facet_df$pvalue[liss_facet_df$facet == "cp09b044"] <- tidy(summary(lm(pintr_dif ~ cp09b044, data=liss)))[2, c("p.value")]
liss_facet_df$estimate[liss_facet_df$facet == "cp09b054"] <- tidy(summary(lm(pintr_dif ~ cp09b054, data=liss)))[2, c("estimate")]
liss_facet_df$stderror[liss_facet_df$facet == "cp09b054"] <- tidy(summary(lm(pintr_dif ~ cp09b054, data=liss)))[2, c("std.error")]
liss_facet_df$pvalue[liss_facet_df$facet == "cp09b054"] <- tidy(summary(lm(pintr_dif ~ cp09b054, data=liss)))[2, c("p.value")]
liss_facet_df$estimate[liss_facet_df$facet == "cp09b059"] <- tidy(summary(lm(pintr_dif ~ cp09b059, data=liss)))[2, c("estimate")]
liss_facet_df$stderror[liss_facet_df$facet == "cp09b059"] <- tidy(summary(lm(pintr_dif ~ cp09b059, data=liss)))[2, c("std.error")]
liss_facet_df$pvalue[liss_facet_df$facet == "cp09b059"] <- tidy(summary(lm(pintr_dif ~ cp09b059, data=liss)))[2, c("p.value")]
liss_facet_df$estimate[liss_facet_df$facet == "cp09b064"] <- tidy(summary(lm(pintr_dif ~ cp09b064, data=liss)))[2, c("estimate")]
liss_facet_df$stderror[liss_facet_df$facet == "cp09b064"] <- tidy(summary(lm(pintr_dif ~ cp09b064, data=liss)))[2, c("std.error")]
liss_facet_df$pvalue[liss_facet_df$facet == "cp09b064"] <- tidy(summary(lm(pintr_dif ~ cp09b064, data=liss)))[2, c("p.value")]
liss_facet_df$estimate[liss_facet_df$facet == "cp09b069"] <- tidy(summary(lm(pintr_dif ~ cp09b069, data=liss)))[2, c("estimate")]
liss_facet_df$stderror[liss_facet_df$facet == "cp09b069"] <- tidy(summary(lm(pintr_dif ~ cp09b069, data=liss)))[2, c("std.error")]
liss_facet_df$pvalue[liss_facet_df$facet == "cp09b069"] <- tidy(summary(lm(pintr_dif ~ cp09b069, data=liss)))[2, c("p.value")]
liss_facet_df$estimate[liss_facet_df$facet == "cp09b029rc"] <- tidy(summary(lm(pintr_dif ~ cp09b029rc, data=liss)))[2, c("estimate")]
liss_facet_df$stderror[liss_facet_df$facet == "cp09b029rc"] <- tidy(summary(lm(pintr_dif ~ cp09b029rc, data=liss)))[2, c("std.error")]
liss_facet_df$pvalue[liss_facet_df$facet == "cp09b029rc"] <- tidy(summary(lm(pintr_dif ~ cp09b029rc, data=liss)))[2, c("p.value")]
liss_facet_df$estimate[liss_facet_df$facet == "cp09b039rc"] <- tidy(summary(lm(pintr_dif ~ cp09b039rc, data=liss)))[2, c("estimate")]
liss_facet_df$stderror[liss_facet_df$facet == "cp09b039rc"] <- tidy(summary(lm(pintr_dif ~ cp09b039rc, data=liss)))[2, c("std.error")]
liss_facet_df$pvalue[liss_facet_df$facet == "cp09b039rc"] <- tidy(summary(lm(pintr_dif ~ cp09b039rc, data=liss)))[2, c("p.value")]
liss_facet_df$estimate[liss_facet_df$facet == "cp09b049rc"] <- tidy(summary(lm(pintr_dif ~ cp09b049rc, data=liss)))[2, c("estimate")]
liss_facet_df$stderror[liss_facet_df$facet == "cp09b049rc"] <- tidy(summary(lm(pintr_dif ~ cp09b049rc, data=liss)))[2, c("std.error")]
liss_facet_df$pvalue[liss_facet_df$facet == "cp09b049rc"] <- tidy(summary(lm(pintr_dif ~ cp09b049rc, data=liss)))[2, c("p.value")]

liss_facet_df %>% 
  select(-facet) %>%
  xtable(.) %>%
  print(., 
        type="html", 
        file="tabA5.html")

stargazer(reg.1, reg.2, reg.3,
          covariate.labels = c("Openness", "Conscientiousness", "Extraversion", "Agreeableness", "Neuroticism", 
                               "Male", "Age", "Edu: Intermediate secondary", "Edu: Higher secondary", "Edu: Intermediate vocational",
                               "Edu: Higher vocational", "Edu: University", "Edu: Other", "Edu: Not completed", "Edu: Not started", "Ideology"),
          align = TRUE,
          single.row = TRUE,
          column.sep.width = "0pt",
          no.space = TRUE,
          digits = 2,
          type = "text",
          out="tabA4.htm")

liss_change <- ggplot(liss, aes(x=pintr_dif)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous("", labels=percent) +
  scale_x_continuous("", breaks=c(-4, 0, 4), label=c("                              Less", "Same", "More")) +
  ggtitle("(A) Political interest during campaign") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) 


coefs = as.data.frame(summary(reg.2)$coefficients[-1,1:2])
names(coefs)[2] = "se" 
coefs$vars = rownames(coefs)
coefs <- coefs[1:5,]
coefs$name <- recode(coefs$vars,
                     `O_sd` = "Openness",
                     `C_sd` = "Conscientiousness",
                     `E_sd` = "Extraversion",
                     `A_sd` = "Agreeableness",
                     `N_sd` = "Neuroticism"
)

coefs$name <- factor(coefs$name, levels = coefs$name)

liss_effect <- ggplot(coefs, aes(name, Estimate)) +
  geom_hline(yintercept=0, col="gray70") +
  geom_errorbar(aes(ymin=Estimate-1.645*se, ymax=Estimate+1.645*se), colour="black", size=1, width=0) +
  geom_errorbar(aes(ymin=Estimate-1.96*se, ymax=Estimate+1.96*se), colour="black", width=0) + 
  geom_point(size = 6, colour = "white") + 
  geom_point(size = 3, shape = 1) +
  scale_y_continuous("") +
  scale_x_discrete("") +
  ggtitle("(B) Effect on difference in interest") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) 

grid.arrange(liss_change, liss_effect, ncol=2) %T>%
  ggsave("fig1.pdf", plot = ., height=4, width=8) %T>%
  ggsave("fig1.png", plot = ., height=4, width=8)

selects %>% 
  filter(!is.na(pintr_dif) & 
           !is.na(O_sd) & 
           !is.na(C_sd) &
           !is.na(E_sd) &
           !is.na(A_sd) &
           !is.na(N_sd)) %>%
  select(polint_pre, polint_post, O_sd, C_sd, E_sd, A_sd, N_sd, male, age, education, ideology) %>%
  data.frame() %>% 
  stargazer(out="tabB2.htm",
            covariate.labels = c("Political interest, campaign", 
                                 "Political interest, non-campaign",
                                 "Openness",
                                 "Conscientiousness", "Extraversion", "Agreeableness",
                                 "Neuroticism", "Male", "Age", "Education", "Ideology"),
            median = TRUE, iqr = TRUE,
            title = "Summary statistics, SELECTS",
            digits = 2, type="text")

selects %>% 
  filter(!is.na(pintr_dif) & 
           !is.na(O_sd) & 
           !is.na(C_sd) &
           !is.na(E_sd) &
           !is.na(A_sd) &
           !is.na(N_sd)) %>%
  select(polint_pre, polint_post, O_sd, C_sd, E_sd, A_sd, N_sd, male, age, education) %>%
  cor(., use="pairwise.complete.obs") %>%
  stargazer(., 
            covariate.labels = c("Political interest, campaign", 
                                 "Political interest, non-campaign",
                                 "Openness",
                                 "Conscientiousness", "Extraversion", "Agreeableness",
                                 "Neuroticism", "Male", "Age", "Education"),
            type="text", digits = 2, out="tabB3.htm")

reg.1 <- lm(pintr_dif ~ O_sd, data=selects)
reg.2 <- lm(pintr_dif ~ O_sd + C_sd + E_sd + A_sd + N_sd + male + age + education, data=selects)
reg.3 <- lm(pintr_dif ~ O_sd + C_sd + E_sd + A_sd + N_sd + male + age + education + ideology, data=selects)

stargazer(reg.1, reg.2, reg.3,
          covariate.labels = c("Openness", "Conscientiousness", "Extraversion", "Agreeableness", "Neuroticism", "Male", "Age", "Education", "Ideology"),
          align = TRUE,
          column.sep.width = "0pt",
          no.space = TRUE,
          single.row = TRUE,
          digits = 2,
          type = "text",
          out="tabB4.htm")

selects_facet_df <- data.frame(facet = c("W3_f15771a", "W3_f15771f", "W3_f15770d"),
                               name = c("Appreciates artistic experiences", "Highly imaginative", "Original and has new ideas"),
                               estimate = NA,
                               stderror = NA,
                               pvalue = NA)

selects_facet_df$estimate[selects_facet_df$facet == "W3_f15771a"] <- tidy(summary(lm(pintr_dif ~ two_sd(W3_f15771a), data=selects)))[2, c("estimate")]
selects_facet_df$stderror[selects_facet_df$facet == "W3_f15771a"] <- tidy(summary(lm(pintr_dif ~ two_sd(W3_f15771a), data=selects)))[2, c("std.error")]
selects_facet_df$pvalue[selects_facet_df$facet == "W3_f15771a"] <- tidy(summary(lm(pintr_dif ~ two_sd(W3_f15771a), data=selects)))[2, c("p.value")]
selects_facet_df$estimate[selects_facet_df$facet == "W3_f15771f"] <- tidy(summary(lm(pintr_dif ~ two_sd(W3_f15771f), data=selects)))[2, c("estimate")]
selects_facet_df$stderror[selects_facet_df$facet == "W3_f15771f"] <- tidy(summary(lm(pintr_dif ~ two_sd(W3_f15771f), data=selects)))[2, c("std.error")]
selects_facet_df$pvalue[selects_facet_df$facet == "W3_f15771f"] <- tidy(summary(lm(pintr_dif ~ two_sd(W3_f15771f), data=selects)))[2, c("p.value")]
selects_facet_df$estimate[selects_facet_df$facet == "W3_f15770d"] <- tidy(summary(lm(pintr_dif ~ two_sd(W3_f15770d), data=selects)))[2, c("estimate")]
selects_facet_df$stderror[selects_facet_df$facet == "W3_f15770d"] <- tidy(summary(lm(pintr_dif ~ two_sd(W3_f15770d), data=selects)))[2, c("std.error")]
selects_facet_df$pvalue[selects_facet_df$facet == "W3_f15770d"] <- tidy(summary(lm(pintr_dif ~ two_sd(W3_f15770d), data=selects)))[2, c("p.value")]

selects_facet_df %>% 
  select(-facet) %>%
  xtable(.) %>%
  print(., 
        type="html", 
        file="tabB5.html")


shp %>% 
  filter(!is.na(pintr) & 
           !is.na(election) & 
           !is.na(O_sd) & 
           !is.na(C_sd) &
           !is.na(E_sd) &
           !is.na(A_sd) &
           !is.na(N_sd)) %>%
  select(election, pintr, pintr_placebo, male, age, edu, ideology, O_sd, C_sd, E_sd, A_sd, N_sd) %>%
  data.frame() %>% 
  stargazer(out="tabC2.htm",
            covariate.labels = c("Election", "Political interest", 
                                 "Political interest, placebo",
                                 "Male", "Age", "Education", "Ideology",
                                 "Openness",
                                 "Conscientiousness", "Extraversion", "Agreeableness",
                                 "Neuroticism"),
            median = TRUE, iqr = TRUE,
            title = "Summary statistics, SHP",
            digits = 2, type="text")

shp %>% 
  filter(!is.na(pintr) & 
           !is.na(election) & 
           !is.na(O_sd) & 
           !is.na(C_sd) &
           !is.na(E_sd) &
           !is.na(A_sd) &
           !is.na(N_sd)) %>%
  select(election, pintr, pintr_placebo, male, age, edu, O_sd, C_sd, E_sd, A_sd, N_sd) %>%
  cor(., use="pairwise.complete.obs") %>%
  stargazer(., 
            covariate.labels = c("Election", "Political interest", 
                                 "Political interest, placebo",
                                 "Male", "Age", "Education",
                                 "Openness",
                                 "Conscientiousness", "Extraversion", "Agreeableness",
                                 "Neuroticism"),
            type="text", digits = 2, out="tabC3.htm")


reg_selects_1 <- lm(pintr_dif ~ O_sd, data=selects)
reg_selects_2 <- lm(pintr_dif ~ O_sd + C_sd + E_sd + A_sd + N_sd, data=selects)

coefs = as.data.frame(summary(reg_selects_2)$coefficients[-1,1:2])
names(coefs)[2] = "se" 
coefs$vars = rownames(coefs)

coefs$name <- recode(coefs$vars,
                     `O_sd` = "Openness",
                     `C_sd` = "Conscientiousness",
                     `E_sd` = "Extraversion",
                     `A_sd` = "Agreeableness",
                     `N_sd` = "Neuroticism"
)

coefs$name <- factor(coefs$name, levels = coefs$name)

selects_effect <- ggplot(coefs, aes(name, Estimate)) +
  geom_hline(yintercept=0, col="gray70") +
  geom_errorbar(aes(ymin=Estimate-1.645*se, ymax=Estimate+1.645*se), colour="black", size=1, width=0) +
  geom_errorbar(aes(ymin=Estimate-1.96*se, ymax=Estimate+1.96*se), colour="black", width=0) + 
  geom_point(size = 6, colour = "white") + 
  geom_point(size = 3, shape = 1) +
  scale_y_continuous("") +
  labs(title = "(A) Effect on difference in political interest",
       x = "Study 2a") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) 

shp_reg_1 <- lm(pintr ~ election + male + age + edu, data=shp)
shp_reg_2 <- lm(pintr ~ election + male + age + edu + O_sd*election, data=shp)
shp_reg_3 <- lm(pintr ~ election + male + age + edu + O_sd*election + C_sd*election + E_sd*election + A_sd*election + N_sd*election, data=shp)
shp_reg_4 <- lm(pintr ~ election + male + age + edu + ideology + O_sd*election + C_sd*election + E_sd*election + A_sd*election + N_sd*election, data=shp)

stargazer(shp_reg_1, shp_reg_2, shp_reg_3, shp_reg_4,
          covariate.labels = c("Election", "Male", "Age", "Education", "Ideology",
                               "Openness", "Conscientiousness", 
                               "Extraversion", 
                               "Agreeableness",
                               "Neuroticism",
                               "Openness * Election",
                               "Conscientiousness * Election",
                               "Extraversion * Election",
                               "Agreeableness * Election",
                               "Neuroticism * Election"
          ),
          align = TRUE,
          column.sep.width = "0pt",
          no.space = TRUE,
          single.row = TRUE,
          digits = 2,
          type = "text",
          out="tabC4.htm")

shp_reg_marg <- interplot(m = shp_reg_3, var1 = "election", var2 = "O_sd", plot=FALSE)

shp_effect <- ggplot(shp_reg_marg, aes(x = O_sd)) +
  geom_hline(yintercept=0, col="gray70") +
  geom_line(aes(y = coef)) +
  geom_line(aes(y = ub), linetype=2) +
  geom_line(aes(y = lb), linetype=2) +
  scale_x_continuous(name="Openness\n \n \n  Study 2b", breaks=c(-1.688, -1.018, -0.348, 0.322, 0.992), label=c("Low", "", "", "", "High")) +
  scale_y_continuous(name="Marginal effect of election") +
  labs() +
  ggtitle("(B) Marginal effect on political interest") 


grid.arrange(selects_effect, shp_effect, ncol=2) %T>%
  ggsave("fig2.pdf", plot = ., height=4, width=8) %T>%
  ggsave("fig2.png", plot = ., height=4, width=8)


shp_placebo_1 <- lm(pintr_placebo ~ O_sd*placebo, data=shp)
shp_placebo_2 <- lm(pintr_placebo ~ O_sd*election, data=shp)
shp_placebo_3 <- lm(pintr ~ O_sd*placebo, data=shp)

stargazer(shp_placebo_1, shp_placebo_2, shp_placebo_3, 
          covariate.labels = c("Openness", "Placebo", "Openness * Placebo", "Election", "Openness * Election"),
          align = TRUE,
          column.sep.width = "0pt",
          no.space = TRUE,
          digits = 2,
          type = "text",
          out="tabC5.htm")


shp_reg_4 <- lm(pintr ~ election + male*election + age + edu + ideology + O_sd*election + C_sd + E_sd + A_sd + N_sd, data=shp)
shp_reg_5 <- lm(pintr ~ election + male + age*election + edu + ideology + O_sd*election + C_sd + E_sd + A_sd + N_sd, data=shp)
shp_reg_6 <- lm(pintr ~ election + male + age + edu*election + ideology + O_sd*election + C_sd + E_sd + A_sd + N_sd, data=shp)
shp_reg_7 <- lm(pintr ~ election + male + age + edu + ideology*election + O_sd*election + C_sd + E_sd + A_sd + N_sd, data=shp)
shp_reg_8 <- lm(pintr ~ election + male*election + age*election + edu*election + ideology*election + O_sd*election + C_sd*election + E_sd*election + A_sd*election + N_sd*election, data=shp)


stargazer(shp_reg_4, shp_reg_5, shp_reg_6, shp_reg_7, shp_reg_8,
          covariate.labels = c("Election", "Male", "Age", "Education", "Ideology",
                               "Openness", "Conscientiousness", 
                               "Extraversion", 
                               "Agreeableness",
                               "Neuroticism",
                               "Male * Election",
                               "Age * Election",
                               "Education * Election",
                               "Ideology * Election",
                               "Openness * Election",
                               "Conscientiousness * Election",
                               "Extraversion * Election",
                               "Agreeableness * Election",
                               "Neuroticism * Election"
          ),
          align = TRUE,
          single.row = TRUE,
          column.sep.width = "0pt",
          no.space = TRUE,
          digits = 2,
          type = "text",
          out="tabC6.htm")

shp_facet_df <- data.frame(facet = c("p09c64", "p09c69"),
                           name = c("Imagination", "Artistic interests"),
                           estimate = NA,
                           stderror = NA,
                           pvalue = NA)



shp_facet_df$estimate[shp_facet_df$facet == "p09c64"] <- tidy(summary(lm(pintr ~ two_sd(p09c64)*election, data=shp)))[4, c("estimate")]
shp_facet_df$stderror[shp_facet_df$facet == "p09c64"] <- tidy(summary(lm(pintr ~ two_sd(p09c64)*election, data=shp)))[4, c("std.error")]
shp_facet_df$pvalue[shp_facet_df$facet == "p09c64"] <- tidy(summary(lm(pintr ~ two_sd(p09c64)*election, data=shp)))[4, c("p.value")]
shp_facet_df$estimate[shp_facet_df$facet == "p09c69"] <- tidy(summary(lm(pintr ~ two_sd(p09c69)*election, data=shp)))[4, c("estimate")]
shp_facet_df$stderror[shp_facet_df$facet == "p09c69"] <- tidy(summary(lm(pintr ~ two_sd(p09c69)*election, data=shp)))[4, c("std.error")]
shp_facet_df$pvalue[shp_facet_df$facet == "p09c69"] <- tidy(summary(lm(pintr ~ two_sd(p09c69)*election, data=shp)))[4, c("p.value")]

shp_facet_df %>% 
  select(-facet) %>%
  xtable(.) %>%
  print(., 
        type="html", 
        file="tabC7.html")


bhps %>% 
  filter(!is.na(pintr) & 
           !is.na(terror) & 
           !is.na(O_sd) & 
           !is.na(C_sd) &
           !is.na(E_sd) &
           !is.na(A_sd) &
           !is.na(N_sd)) %>%
  select(terror, pintr, pintr_placebo, male, age, edu, O_sd, C_sd, E_sd, A_sd, N_sd) %>%
  data.frame() %>% 
  stargazer(out="tabD2.htm",
            covariate.labels = c("Terror", "Political interest", 
                                 "Political interest, placebo",
                                 "Male", "Age", "Education",
                                 "Openness",
                                 "Conscientiousness", "Extraversion", "Agreeableness",
                                 "Neuroticism"),
            median = TRUE, iqr = TRUE,
            title = "Summary statistics, BHPS",
            digits = 2, type="text")


bhps %>% 
  filter(!is.na(pintr) & 
           !is.na(terror) & 
           !is.na(O_sd) & 
           !is.na(C_sd) &
           !is.na(E_sd) &
           !is.na(A_sd) &
           !is.na(N_sd)) %>%
  select(terror, pintr, pintr_placebo, male, age, edu, O_sd, C_sd, E_sd, A_sd, N_sd) %>%
  cor(., use="pairwise.complete.obs") %>%
  stargazer(., 
            covariate.labels = c("Terror", "Political interest", 
                                 "Political interest, placebo",
                                 "Male", "Age", "Education",
                                 "Openness",
                                 "Conscientiousness", "Extraversion", "Agreeableness",
                                 "Neuroticism"),
            type="text", digits = 2, out="tabD3.htm")

bhps_reg_1 <- lm(pintr ~ terror, data=bhps)
bhps_reg_2 <- lm(pintr ~ terror*O_sd, data=bhps)
bhps_reg_3 <- lm(pintr ~ terror*O_sd + terror*C_sd + terror*E_sd + terror*A_sd + terror*N_sd + male + age + factor(edu), data=bhps)
bhps_reg_4 <- lm(pintr ~ terror*O_sd + terror*C_sd + terror*E_sd + terror*A_sd + terror*N_sd + male + age + factor(edu) + factor(party), data=bhps)

stargazer(bhps_reg_1, bhps_reg_2, bhps_reg_3, bhps_reg_4,
          keep = c("terror", "O_sd", "C_sd", "E_sd", "A_sd", "N_sd", "male", "age"),
          covariate.labels = c("Terror", 
                               "Openness", "Conscientiousness", 
                               "Extraversion", 
                               "Agreeableness",
                               "Neuroticism",
                               "Male", "Age", 
                               "Openness * Terror",
                               "Conscientiousness * Terror",
                               "Extraversion * Terror",
                               "Agreeableness * Terror",
                               "Neuroticism * Terror"
          ),
          align = TRUE,
          column.sep.width = "0pt",
          no.space = TRUE,
          digits = 2,
          type = "text",
          out="tabD4.htm")

bhps_coefs <- as.data.frame(summary(bhps_reg_3)$coefficients[-1,1:2])
names(bhps_coefs) <- c("est", "se")
bhps_coefs$vars = rownames(bhps_coefs)

bhps_coefs <- bhps_coefs %>%
  filter(vars %in% c("terror:O_sd", "terror:C_sd", "terror:E_sd", "terror:A_sd", "terror:N_sd"))

bhps_coefs$name <- recode(bhps_coefs$vars,
                          `terror:O_sd` = "Openness \n× terror",
                          `terror:C_sd` = "Conscientiousness \n× terror",
                          `terror:E_sd` = "Extraversion \n× terror",
                          `terror:A_sd` = "Agreeableness \n× terror",
                          `terror:N_sd` = "Neuroticism \n× terror"
)

bhps_coefs$name <- factor(bhps_coefs$name, levels = c("Neuroticism \n× terror", 
                                                      "Agreeableness \n× terror",
                                                      "Extraversion \n× terror",
                                                      "Conscientiousness \n× terror",
                                                      "Openness \n× terror"))

bhps_int <- ggplot(bhps_coefs, aes(name, est)) + 
  geom_hline(yintercept=0, col="gray70") +
  geom_errorbar(aes(ymin=est-1.645*se, ymax=est+1.645*se), colour="black", size=1, width=0) +
  geom_errorbar(aes(ymin=est-1.96*se, ymax=est+1.96*se), colour="black", width=0) + 
  geom_point(size = 6, colour = "white") + 
  geom_point(size = 3, shape = 1) +
  scale_y_continuous("Coefficient", breaks=c(-0.25, 0.00, 0.25, 0.50), labels=c("-0.25", "0.00", "0.25", "0.50")) +
  scale_x_discrete("") +
  ggtitle("(A) Interaction results") +
  coord_flip()

bhps_reg_marg <- interplot(m =bhps_reg_3, var1 = "terror", var2 = "O_sd", plot=FALSE)

bhps_open <- ggplot(bhps_reg_marg, aes(x = O_sd)) +
  geom_hline(yintercept=0, col="gray70") +
  geom_line(aes(y = coef)) +
  geom_line(aes(y = ub), linetype=2) +
  geom_line(aes(y = lb), linetype=2) +
  scale_x_continuous(name="Openness", breaks=c(-1,-0.50,0,0.50,1), 
                     labels=c("Low", "", "", "", "High")) +
  scale_y_continuous(name="Marginal effect of 9/11") +
  ggtitle("(B) Marginal effect on political interest") 

grid.arrange(bhps_int, bhps_open, ncol=2) %T>%
  ggsave("fig3.pdf", plot = ., height=4, width=8) %T>%
  ggsave("fig3.png", plot = ., height=4, width=8)


reg_bhps_placebo_1 <- lm(pintr_placebo ~ placebo*O_sd, data=bhps)
reg_bhps_placebo_2 <- lm(pintr ~ placebo*O_sd, data=bhps)
reg_bhps_placebo_3 <- lm(pintr_placebo ~ terror*O_sd, data=bhps)

stargazer(reg_bhps_placebo_1, reg_bhps_placebo_2, reg_bhps_placebo_3, 
          covariate.labels = c("Placebo", "Terror", "Openness", "Openness * Placebo", "Openness * Terror"),
          align = TRUE,
          column.sep.width = "0pt",
          no.space = TRUE,
          digits = 2,
          type = "text",
          out="tabD5.htm")


bhps_reg_4 <- lm(pintr ~ terror*O_sd + terror*C_sd + terror*E_sd + terror*A_sd + terror*N_sd + male*terror + age + factor(edu), data=bhps)
bhps_reg_5 <- lm(pintr ~ terror*O_sd + terror*C_sd + terror*E_sd + terror*A_sd + terror*N_sd + male + age*terror + factor(edu), data=bhps)
bhps_reg_6 <- lm(pintr ~ terror*O_sd + terror*C_sd + terror*E_sd + terror*A_sd + terror*N_sd + male + age + factor(edu)*terror, data=bhps)
bhps_reg_7 <- lm(pintr ~ terror*O_sd + terror*C_sd + terror*E_sd + terror*A_sd + terror*N_sd + male*terror + age*terror + factor(edu)*terror, data=bhps)

stargazer(bhps_reg_4, bhps_reg_5, bhps_reg_6, bhps_reg_7, 
          omit = c("terror:factor", "factor", "terror:male", "terror:age"),
          covariate.labels = c("Terror", 
                               "Openness", "Conscientiousness", 
                               "Extraversion", 
                               "Agreeableness",
                               "Neuroticism",
                               "Male", "Age", 
                               "Openness * Terror",
                               "Conscientiousness * Terror",
                               "Extraversion * Terror",
                               "Agreeableness * Terror",
                               "Neuroticism * Terror"
          ),
          align = TRUE,
          single.row = TRUE,
          column.sep.width = "0pt",
          no.space = TRUE,
          digits = 2,
          type = "text",
          out="tabD6.htm")

bhps_facet_df <- data.frame(facet = c("optrt5o1", "optrt5o2", "optrt5o3"),
                            name = c("Original, come up with ideas", "Values artistic experiences", "Active imagination"),
                            estimate = NA,
                            stderror = NA,
                            pvalue = NA)

bhps_facet_df$estimate[bhps_facet_df$facet == "optrt5o1"] <- tidy(summary(lm(pintr ~ two_sd(optrt5o1)*terror, data=bhps)))[4, c("estimate")]
bhps_facet_df$stderror[bhps_facet_df$facet == "optrt5o1"] <- tidy(summary(lm(pintr ~ two_sd(optrt5o1)*terror, data=bhps)))[4, c("std.error")]
bhps_facet_df$pvalue[bhps_facet_df$facet == "optrt5o1"] <- tidy(summary(lm(pintr ~ two_sd(optrt5o1)*terror, data=bhps)))[4, c("p.value")]
bhps_facet_df$estimate[bhps_facet_df$facet == "optrt5o2"] <- tidy(summary(lm(pintr ~ two_sd(optrt5o2)*terror, data=bhps)))[4, c("estimate")]
bhps_facet_df$stderror[bhps_facet_df$facet == "optrt5o2"] <- tidy(summary(lm(pintr ~ two_sd(optrt5o2)*terror, data=bhps)))[4, c("std.error")]
bhps_facet_df$pvalue[bhps_facet_df$facet == "optrt5o2"] <- tidy(summary(lm(pintr ~ two_sd(optrt5o2)*terror, data=bhps)))[4, c("p.value")]
bhps_facet_df$estimate[bhps_facet_df$facet == "optrt5o3"] <- tidy(summary(lm(pintr ~ two_sd(optrt5o3)*terror, data=bhps)))[4, c("estimate")]
bhps_facet_df$stderror[bhps_facet_df$facet == "optrt5o3"] <- tidy(summary(lm(pintr ~ two_sd(optrt5o3)*terror, data=bhps)))[4, c("std.error")]
bhps_facet_df$pvalue[bhps_facet_df$facet == "optrt5o3"] <- tidy(summary(lm(pintr ~ two_sd(optrt5o3)*terror, data=bhps)))[4, c("p.value")]

bhps_facet_df %>% 
  select(-facet) %>%
  xtable(.) %>%
  print(., 
        type="html", 
        file="tabD7.html")

sink("sessionInfo.txt") 
cat("\nThe results are produced with 02-analysis.R with this session in R:\n\n")
sessionInfo()
sink() 
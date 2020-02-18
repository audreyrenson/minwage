library(tidyverse)
library(quantreg)
library(magrittr)
w3grp <- haven::read_xpt("../../../addhealthdata/wave3/w3group.xpt")
w4grp <- haven::read_xpt("../../../addhealthdata/wave4/w4group.xpt")  #w4group0 is census 2000, w4group1 is census 2010
w3 <- haven::read_xpt("../../../addhealthdata/wave3/wave3.xpt")
w4 <- haven::read_xpt("../../../addhealthdata/wave4/wave4.xpt")
mw <- read_csv("VZ_mw_state_quarterly_new.csv")


#show minimum wage laws by year implemented
mw %>% 
  mutate(year = 1960 + quarterly_date/4) %>%
  ggplot(aes(x=year, y=max_mw)) + 
  geom_point()

#get changes between waves - this is not very exact because participants were interviewed at different times
mw_changes <- mw %>% 
  mutate(year = 1960 + quarterly_date/4) %>%
  select(statefips, statename, year, max_mw) %>%
  filter(year >2000 & year < 2008) %>%
  group_by(statefips, statename) %>%
  mutate(lag_max_mw = lag(max_mw)) %>%
  filter(lag(max_mw) != max_mw) %>%
  mutate(start_mw = lag_max_mw[year == min(year)],
         diff_mw   = max_mw[year == max(year)] - start_mw) %>%
  select(statefips, statename, start_mw, diff_mw) %>%
  unique

#DATA MANAGEMENT
w3state <- w3grp %>% 
  mutate(statefips = as.integer(str_sub(W3GROUP, 1, 2))) %>%
  left_join(mw %>% select(statefips, statename) %>% unique) %>%
  select(AID, statefips, statename)
w3_ <- w3 %>%
  select(AID, IYEAR3, IMONTH3, IDAY3,
         H3EC2, #Including all the income sources you reported above, what was your total personal income before taxes in {2000/2001}?
         #999996 = refused,  999998=don't know, 999999=not applicable
         H3EC3,	#What is your best guess of your total personal income before taxes?
         #1=less than $10,000, 2=$10,000 to $14,999, 3=$15,000 to $19,999, 4=$20,000 to $29,999, 
         #5=$30,000 to $39,999, 6=$40,000 to $49,999, 7=$50,000 to $74,999, 8=$75,000 or more, 
         #96=refused, 97=legitimate skip, 98=don't know, 99=not applicable, 98=don't know
         H3EC4, #In {2000/2001}, how much income did you receive from earnings--that is, wages or salaries, including tips, bonuses, and overtime pay, and income from self-employment?
         #999996 = refused, 999997=legitimate skip, 999998=don't know, 999999=not applicable
         H3EC5, #What is your best guess of the income you received from earnings?
         #1=less than $10,000 2=$10,000 to $14,999,3=$15,000 to $19,999,
         #4=$20,000 to $29,999, 5=30,000 to $39,999, 6=$40,000 to $49,999,
         #7=$50,000 to $74,999, 8=$75,000 or more, 
         #96=refused, 97=legitimate skip, 98=don't know, 99=not applicable
         H3EC6, #what was the total household income before taxes in {2000/2001}? (Thinking about your income and the income of everyone who lives in your household and contributes to the household budget)
         #999996=refused, 999997=legitimate skip, 999998=don't know, 999999=not applicable
         H3EC7,  #What is your best guess of the total household income before taxes?
         #1=less than $10,000, 2=$10,000 to $14,999, 3=$15,000 to $19,999, 4=$20,000 to $29,999, 
         #5=$30,000 to $39,999, 6=$40,000 to $49,999, 7=$50,000 to $74,999, 8=$75,000 or more, 
         #96=refused, 97=legitimate skip, 98=don't know, 99=not applicable, 98=don't know
         H3EC8, #what was your total household income before taxes in {2000/2001}? (Thinking about your income and the income of your spouse or partner, and all types of income sources)
         #999996=refused, 999997=legitimate skip, 999998=don't know, 999999=not applicable, 
         H3EC9 #What is your best guess of your total household income before taxes?
         #1=less than $10,000, 2=$10,000 to $14,999, 3=$15,000 to $19,999, 4=$20,000 to $29,999, 5=$30,000 to $39,999, 6=$40,000 to $49,999, 7=$50,000 to $74,999, 8=$75,000 or more, 96=refused, 97=legitimate skip, 98=don't know, 99=not applicable
  ) %>% 
  left_join(w3state)


w4state <- w4grp %>% 
  mutate(statefips = as.integer(str_sub(W4GROUP0, 1, 2))) %>%
  left_join(mw %>% select(statefips, statename) %>% unique)  %>% 
  select(AID, statefips, statename)
w4_ <- w4 %>%
  select(AID, IYEAR4, IMONTH4, IDAY4,
         H4EC1, #what was the total household income before taxes and deductions in {2006/2007/2008}
         #1=less than $5,000, 2=$5,000 to $9,999, 3=$10,000 to $14,999, 4=$15,000 to $19,999, 
         #5=$20,000 to $24,999, 6=$25,000 to $29,999, 7=$30,000 to $39,999, 8=$40,000 to $49,999, 
         #9=$50,000 to $74,999, 10=$75,000 to $99,999, 11=$100,000 to $149,999, 12=$150,000 or more, 
         #96=refused, 98=don't know,
         H4EC2, # In {2006/2007/2008}, how much income did you receive from personal earnings before taxes, that is, wages or salaries, including tips, bonuses, and overtime pay, and income from self-employment?
         #9999996=refused, 99999998=don't know
         H4EC3 #What is your best guess of your personal earnings before taxes?
         #1=less than $5,000, 2=$5,000 to $9,999, 3=$10,000 to $14,999, 4=$15,000 to $19,999, 
         #5=$20,000 to $24,999, 6=$25,000 to $29,999, 7=$30,000 to $39,999, 8=$40,000 to $49,999, 
         #9=$50,000 to $74,999, 10=$75,000 to $99,999, 11=$100,000 to $149,999, 12=$150,000 or more, 
         #96=refused, 97=legitimate skip, 98=don't know
  ) %>% 
  mutate(H4EC1 = ifelse(H4EC1 >95, NA, H4EC1), #code missings
         H4EC2 = ifelse(H4EC2 > 9999995, NA, H4EC2),
         H4EC3 = ifelse(H4EC3 > 95, NA, H4EC3)) %>% 
  left_join(w4state)

#####interpolate using the empirical density
w4_ <- w4_ %>%
  mutate(w4_hi_unif = case_when(H4EC1==1 ~ runif(nrow(.), 0,     4999),
                                H4EC1==2 ~ runif(nrow(.), 5e3,   9999),
                                H4EC1==3 ~ runif(nrow(.), 1e4,  14999),
                                H4EC1==4 ~ runif(nrow(.), 15e3, 19999),
                                H4EC1==5 ~ runif(nrow(.), 2e4,  24999),
                                H4EC1==6 ~ runif(nrow(.), 25e3, 3e4-1),
                                H4EC1==7 ~ runif(nrow(.), 3e4,  4e4-1),
                                H4EC1==8 ~ runif(nrow(.), 4e4,  5e4-1),
                                H4EC1==9 ~ runif(nrow(.), 5e4,  75e3-1),
                                H4EC1==10 ~ runif(nrow(.),75e3, 1e5-1),
                                H4EC1==11 ~ runif(nrow(.), 1e5, 15e4-1),
                                H4EC1==12 ~ runif(nrow(.), 15e4, 2e5))) 
#check bandwidth - 15,000 looks good
str(density(w4_$w4_hi_unif,na.rm = TRUE, bw = 15e3))
n_samp <- 1e5
ed <- tibble(prob_samples = sample(w4_$w4_hi_unif, size=n_samp, replace = TRUE) + rnorm(n_samp,0,15e3),
                            bin = cut(prob_samples, 
                                      breaks=c(0,5e3,1e4,15e3,2e4,25e3,3e4,4e4,5e4,75e3,1e5,15e4,Inf),
                                      labels = 1:12)) %>%
  drop_na()
ggplot(ed, aes(x=prob_samples, fill=bin)) +
  geom_histogram(binwidth = 1000) +
  theme(axis.text.x = element_text(angle=90,hjust=1,vjust=1))
#interpolate
ed_split <- ed %>% split(.$bin) %>% map(pull, prob_samples)
w4_int <- w4_ %>%
  filter(!is.na(H4EC1)) %>%
  mutate(hi_int = map_dbl(H4EC1, ~sample(ed_split[[as.character(.)]], size=1))) 
         


#### match changes
#baseline minimum wage and change from baseline
w4_int <- w4_int %>%
  left_join(mw_changes) %>%
  mutate(x1=log(diff_mw), x2=log(start_mw), y=log(hi_int))

#### what does the relationship look like?
#ols
mod <- lm(y ~ x1 + x2, data=w4_int)
#quantile regression - maybe 10th-50th percentiles
qmod <- rq(y ~ x1 + x2, data=w4_int, tau = seq(.05,.95,.1))

#posterior predictive distribution / quantiles
param_mu <- coef(mod)
param_vc <- vcov(mod)
data_sd <- sqrt(sum(mod$residuals^2)/(mod$df.residual-1))

qp <- w4_int %>% 
  ungroup %>%
  mutate(int = 1) %>%
  select(int, x1, x2, diff_mw, hi_int) %>%
  filter(!is.na(x1)) %>%
  unique %>%
  `[`(sample(1:nrow(.), size=300, replace=TRUE),) %>%
  #add quantiles
  bind_cols(predict(qmod, newdata=.) %>% 
              set_colnames(paste0("q", seq(5,95,10))) %>%
              as_tibble()) %>%
  pivot_longer(q5:q95, names_to = "quantile") %>%
  mutate(quantile = factor(quantile, levels=paste0("q", seq(5,95,10)))) %>%
  mutate(x = map(1:nrow(.), ~c(int[.], x1[.], x2[.])),
         param = replicate(nrow(.), MASS::mvrnorm(n=1, mu=param_mu, Sigma = param_vc), simplify = FALSE),
         mu = map2_dbl(x, param, ~.x %*% .y ),
         y_tilde = mu + rnorm(nrow(.), 0, data_sd)) %>% 
  group_by(x1) %>%
  ggplot(aes(x=x1, y=y_tilde)) +
  geom_jitter(width = .1, alpha=.3) +
  geom_jitter(data= w4_int, aes(y=log(hi_int)), 
              width = .1, alpha=.1, color="dodgerblue") +
  geom_line(aes(y=value, color=quantile)) +
  labs(title="Black=OLS ppd, blue=data", y="log(household income)", x=expression(log(Delta[MW])))


qp


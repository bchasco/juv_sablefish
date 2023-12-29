library(tidyr)
library(dplyr)
library(ggplot2)
library(units)

cat <-   c("Subyearling \nChinook", 
           "Yearling \nChinook",
           "Yearling \nCoho", 
           "Sablefish")

val <- as.data.frame(t(as.list(fit$parameter_estimates$SD, "Estimate", report=TRUE)$Index_ctl[,,1]))
names(val) <- cat

sd <- as.data.frame(t(as.list(fit$parameter_estimates$SD, "Std. Error", report=TRUE)$Index_ctl[,,1]))
names(sd) <- cat

sd$year <- 1998:2023
val$year <- 1998:2023

#You hvae to remove all those sablefish years with no catches.
obs <- data.frame(t = fit$data_frame$t_i, c = fit$data_frame$c_iz, b = fit$data_frame$b_i) 
obs <- obs %>% 
  group_by(t,c) %>%
  summarize(n = sum(b))
obs$n <- drop_units(obs$n)
sable_0yrs <- obs$t[obs$c==3 & obs$n==0]

sd <- sd %>%
  pivot_longer(!c(year), names_to = "cat", values_to = "sd") #%>%
val <- val %>%
  pivot_longer(!c(year), names_to = "cat", values_to = "est") 
val$sd <- sd$sd

ave <- val %>%
  group_by(cat) %>%
  summarize(m = mean(est)) 

# val$est <- exp(val$est - 0.5*val$sd^2)
val$m <- ave$m[match(val$cat,ave$cat)]
val$lwr <- qnorm(0.05,val$est,val$sd)
val$upr <- qnorm(0.95,val$est,val$sd)
#Remove bad sablefish years
# val <- val[!(val$cat=="Sablefish" & (val$year%in%sable_0yrs)),]

g <- ggplot(val, aes(x = year, y = (est/m)-1)) +
  geom_point() +
  geom_errorbar(aes(ymin = (lwr/m)-1,
                ymax = (upr/m)-1)) +
  facet_wrap(~cat, scales = "free") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("Relative index of abundnace") +
  xlab("Calendar year") +
  geom_hline(data = ave,
             aes(yintercept = 0),
             inherit.aes = FALSE,
             linetype = "dashed")
  
print(g)
ggsave("C:\\noaa\\projects\\juv_sablefish\\output\\Index.png",g)


ave_cv <- val %>%
  group_by(cat) %>%
  summarize(cv = mean(sd/est))

ave_max_increase <- val %>%
  group_by(year,cat) %>%
  summarize(max_increase = (est/m - 1)) %>%
  arrange(desc(max_increase))


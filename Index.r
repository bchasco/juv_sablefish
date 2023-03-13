cat <-   c("Subyearling \nChinook", 
           "Yearling \nChinook",
           "Yearling \nCoho", 
           "Sablefish")

val <- as.data.frame(t(as.list(fit$parameter_estimates$SD, "Estimate", report=TRUE)$Index_ctl[,,1]))
names(val) <- cat

sd <- as.data.frame(t(as.list(fit$parameter_estimates$SD, "Std. Error", report=TRUE)$Index_ctl[,,1]))
names(sd) <- cat

sd$year <- 1998:2020
val$year <- 1998:2020

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

val$est[val$cat=="Sablefish"]/mean(val$est[val$cat=="Sablefish"  & val$year!=2020])

#Remove bad sablefish years
val <- val[!(val$cat=="Sablefish" & (val$year%in%sable_0yrs)),]

g <- ggplot(val, aes(x = year, y = est)) +
  geom_point() +
  geom_errorbar(aes(ymin = est - 1.96*sd, ymax = est + 1.96*sd)) +
  facet_wrap(~cat, scales = "free") +
  theme_bw() + 
  ylab("Index of abundnace") +
  xlab("Calendar year") +
  geom_hline(data = ave, 
             aes(yintercept = m), 
             inherit.aes = FALSE,
             linetype = "dashed")
  

png("Index.png")

print(g)
# 
dev.off()


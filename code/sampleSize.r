#Read in the data
df <- as.data.frame(read.csv(file=paste0(getwd(),"/data/survey.csv"), header=TRUE))
df <- df[df$Lat>=44.25 & df$Lat<=48.3,]
df <- df %>% 
  filter(Month == 'June') %>%
  filter(Study == 'JSOES_MaxCatch' | Study == 'JSOES_Regular') %>%
  gather(key="sp", 
         value="catch",
         c(Sablefish_juv, CK_subyearling, CK_yearling, Coho_yearling))


ss <-  df[!duplicated(df[,c('Station','Year')]), c('Station','Year')]
t_ss <- table(df$Station,df$Year)/4
barplot(colSums(t_ss))

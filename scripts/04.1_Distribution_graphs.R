##############################################################################-
# DATE:
#   2024/may/10
# AUTHOR:
#  Jaime Buitrago
# DESCRIPTION:
#   Dsitribution graphs for continuos variables
##############################################################################-

# Prepare workspace

rm (list=ls())
source("scripts/00_packages.R")
gc()

############################################################################-
# 1. Database preparation ----
############################################################################-
stats <- arrow::read_parquet("stores/db3.parquet")
stats <- data.frame(stats%>%
                      filter(is_chapinero=="Chapinero")
)

stats$price <- log(stats$price)

############################################################################-
# 2. Graphs ----
############################################################################-


ggplot(stats, aes(x=price)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="blue")+
  geom_vline(aes(xintercept=mean(price)),
              color="blue", linetype="dashed", size=1)+
  xlab("Precio (log)") +ylab("Densidad")+ 
  theme_bw()

ggsave("views/g1.jpg", scale=1, width = 7, height = 5 , units = 'in', dpi=600)

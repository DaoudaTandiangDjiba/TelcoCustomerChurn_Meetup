names(telco_full)
# Create a data frame for continuous variables
Countinous_variables <- telco_full[,c(6,19,20,21)]
Charges <- data.frame(rep(Countinous_variables[,c(1,2,3)])) 
Countinous_variables_plot <- ggplot(Countinous_variables, aes(x = Charges, y = Churn)) + 
  geom_boxplot()+ 
  theme_bw()+
  xlab(" ")
Countinous_variables_plot
plot_grid(Tenure_plot,MonthlyCharges_plot,TotalCharges_plot)


# 1.Tenure
options(repr.plot.width =6, repr.plot.height = 2)
Tenure_plot <- ggplot(telco_full, aes(y= Tenure, x ="", fill = Churn)) + 
  geom_boxplot()+ 
  theme_bw()+
  xlab("Churn")
Tenure_plot

# 2.MonthlyCharges
options(repr.plot.width =6, repr.plot.height = 2)
MonthlyCharges_plot <- ggplot(telco_full, aes(y= MonthlyCharges, x ="", fill = Churn)) + 
  geom_boxplot()+ 
  theme_bw()+
  xlab("Churn")
MonthlyCharges_plot

# 3.TotalCharges
options(repr.plot.width =6, repr.plot.height = 2)
TotalCharges_plot <- ggplot(telco_full, aes(y= TotalCharges, x ="", fill = Churn)) + 
  geom_boxplot()+ 
  theme_bw()+
  xlab("Churn")



ggplot(telco_full3, aes(x = Tenure_bin, fill = Churn))+
  geom_bar()+ 
  facet_wrap(~Gender)+
  theme_bw()+
  ggtitle("Distribution of Tenure_bin by the churn rate")

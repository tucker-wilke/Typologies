
median_Income <- median(State_Vars_Expanded$Per_Capita_Income)
median_ff_jobs <- median(State_Vars_Expanded$FF_Job_Share_Total, na.rm=TRUE)
median_clean_jobs <- median(State_Vars_Expanded$Clean_Job_Share_Total, na.rm=TRUE)
median_tax_base <- median(State_Vars_Expanded$FF_Percent_Total, na.rm=TRUE)
median_CPI <- median(State_Vars_Expanded$climate_policy_index, na.rm=TRUE)
median_energy_net <-median(State_Vars_Expanded$Net_Energy, na.rm=TRUE)
median_professionalization <-median(State_Vars_Expanded$`Prof 2021`, na.rm=TRUE)
median_service <- median(State_Vars_Expanded$Tertiary_Share, na.rm=TRUE)
median_secondary <-  median(State_Vars_Expanded$Secondary_Share, na.rm=TRUE)
median_primary <- median(State_Vars_Expanded$Primary_Share, na.rm=TRUE)
median_liberal <-median(State_Vars_Expanded$liberal, na.rm=TRUE)
median_governor <- median(State_Vars_Expanded$Governor_should_do_more, na.rm=TRUE)
median_ff_exports <-median(State_Vars_Expanded$Net_FF, na.rm=TRUE)
median_manufacturing <-median(State_Vars_Expanded$Manufacturing_GDP, na.rm=TRUE)
##

Typologies_Analysis <- State_Vars_Expanded %>%
  drop_na(Category)

ggplot(Typologies_Analysis, aes(x = Per_Capita_Income, y = climate_policy_index, color = Category)) +
  geom_point(size = 3) + # Scatter plot points with size
  geom_text(aes(label = State), vjust = -0.5, hjust = 1.5) + # Labels
  labs(x = "Income Per Capita", y = "Climate Policy Index", title = "Climate Policy and Income") +
  geom_vline(xintercept = median_Income, linetype = "dashed", color = "gray") +
  geom_hline(yintercept = median_CPI, linetype = "dashed", color = "gray") +
  theme_minimal()
  

ggplot(Typologies_Analysis, aes(x = FF_Percent_Total, y = FF_Job_Share_Total, color = Category)) +
  geom_point(size = 3) + # Scatter plot points with size
  geom_text(aes(label = State), vjust = -0.5, hjust = 1.5) + # Labels +
  labs(x = "Fossil Fuel Share of Tax Revenue", y = "FF Share of Jobs") +
  geom_hline(yintercept = median_ff_jobs,  linetype = "dashed", color = "red" ) +
  geom_vline(xintercept = median_tax_base, linetype = "dashed", color = "red" ) +
  theme_minimal()

ggplot(Typologies_Analysis, aes(x = Primary_Share, y = climate_policy_index, color = Category)) +
  geom_point(size = 3) + # Scatter plot points with size
  geom_text(aes(label = State), vjust = -0.5, hjust = 1.5) + # Labels
  labs(x = "Primary Share of Economy", y = "Climate Policy Index", title = "Climate Policy vs Primary Sector") +
  geom_vline(xintercept = median_primary, linetype = "dashed", color = "red") +
  geom_hline(yintercept = median_CPI, linetype = "dashed", color = "red") +
  theme_minimal()

ggplot(Typologies_Analysis, aes(x = Secondary_Share, y = climate_policy_index, color = Category)) +
  geom_point(size = 3) + # Scatter plot points with size
  geom_text(aes(label = State), vjust = -0.5, hjust = 1.5) + # Labels
  labs(x = "Secondary Share of Economy", y = "CPI", title = "Climate Policy vs Secodnary Share") +
  geom_vline(xintercept = median_secondary, linetype = "dashed", color = "red") +
  geom_hline(yintercept = median_CPI, linetype = "dashed", color = "red") +
  theme_minimal()

ggplot(Typologies_Analysis, aes(x = Manufacturing_GDP, y = climate_policy_index, color = Category)) +
  geom_point(size = 3) + # Scatter plot points with size
  geom_text(aes(label = State), vjust = -0.5, hjust = 1.5) + # Labels
  labs(x = "Manufacturing Share of GDP", y = "Climate Policy Index", title = "Climate Policy vs Manufacturing Share") +
  geom_vline(xintercept = median_manufacturing, linetype = "dashed", color = "red") +
  geom_hline(yintercept = median_CPI, linetype = "dashed", color = "red") +
  theme_minimal()


ggplot(Typologies_Analysis, aes(x = Tertiary_Share, y = climate_policy_index, color = Category)) +
  geom_point(size = 3) + # Scatter plot points
  geom_text(aes(label = State), vjust = -0.5, hjust = 1.5) + # Labels
  labs(x = "Tertiary Share of Economy", y = "Climate Policy Index", title = "Climate Policy vs Tertiary Share") +
  geom_hline(yintercept = median_CPI,  linetype = "dashed", color = "red" ) +
  geom_vline(xintercept = median_service,  linetype = "dashed", color = "red" ) +
  theme_minimal()


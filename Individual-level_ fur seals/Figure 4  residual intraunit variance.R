library(gridExtra);library(tibble)
library(ggplot2);library(gridExtra)
library(brms);library(dplyr)
library(ggridges);library(tidyr)

#Estimates of posterior distribution residual intra-individual variation (intraunit variance) for 𝛿¹³C and 𝛿15N at the individual level for (a) males and (b) females of fur seals Arctocephalus australis.

model1 <- readRDS("modeloAA.rds")
summary(model1)

# fixed effects
DF<-as.data.frame(fixef(model1))
coef_table <- rownames_to_column(DF, var = "Variable")
head(coef_table)

# random effects
ranefs<-VarCorr(model1)
coef_random <- as.data.frame(ranefs[["ID"]][["sd"]])
coef_table1 <- rownames_to_column(coef_random, var = "Variable")
coef_table1$Variable<- as.factor(coef_table1$Variable)
levels(coef_table1$Variable)
levels(coef_table1$Variable)<- c("Vμ_d13C","Vμ_d15N", "Vω_d13C", "Vω_d15N") 

#plot the posterior distribution of each individual’s
#predicted standard deviation (i.e. rIIV). Individuals with higher rIIV are less predictable than individuals
#with lower rIIV

comunidad1<-posterior_samples(model1)

df_filtrado <- comunidad1 %>%
  select(starts_with("r_ID__sigma_d"))

#In order to interpret rIIV in biological terms we backtransform rIIV by taking
#it’s original scale by taking its exponent (ref: guide for studying among-individual behavioral variation from movement data in the wild. Anne G. Hertel & Petri T. Niemelä, 2020)
community.sp<- exp(df_filtrado)
Sp.C<- community.sp[, c(1:37)] #  each individual for C  
Sp.N<- community.sp[, c(38:74)] # each individual for N
str(Sp.C)
str(Sp.N)

# for Carbon
df_Carbon <- Sp.C %>%
  pivot_longer(cols = everything(), 
               names_to = "ID", 
               values_to = "Intraindividual Variance") %>%
  mutate(ID = gsub("r_ID__sigma_d13C\\[|,Intercept\\]", "", ID))%>%
  arrange(ID)  # order by ID

df_Carbon <- as.data.frame(df_Carbon)
df_Carbon$ID<- as.factor(df_Carbon$ID)

# for Nitrogen

df_Nitrogen <- Sp.N %>%
  pivot_longer(cols = everything(), 
               names_to = "ID", 
               values_to = "Intraindividual Variance") %>%
  mutate(ID = gsub("r_ID__sigma_d15N\\[|,Intercept\\]", "", ID))%>%
  arrange(ID)  # order by ID

df_N <- as.data.frame(df_Nitrogen)
df_N$ID<- as.factor(df_N$ID)

alls<- rbind(df_N,df_Carbon)
nrow(df_N)
nrow(df_Carbon)

Isotope <- factor(c(rep("Nitrogen", 296000), rep("Carbon", 296000)))

# create a dataframe with the variable "isotope" 
df_largo4<- data.frame(Isotope = Isotope)
DF_plot<-cbind(alls,df_largo4)
str(DF_plot)

#divide for sex (males and females separately)

# Select only females
hembras <- c("167",    "168",    "169",    "170",    "172",  "Aa156",  "Aa159",  "Aa181",  "Aa182",  "Aa184",
"Aa185",  "Aa186",  "Aa187",  "Aa188",  "Aa189",  "Aa190",  "Aah10",  "Aah11",  "Aah13",  "Aah14",  "Aah9")
hembras1 <- subset(DF_plot, ID %in% hembras)
head(hembras1)

#intraindividua mean variance females
DF_plot_fem <- hembras1 %>%
  group_by(ID) %>%
  mutate(Mean_IV = mean(`Intraindividual Variance`)) %>%
  ungroup()
#Order ID by mean intraindividual variance
DF_plot_fem$ID <- factor(DF_plot_fem$ID, levels = DF_plot_fem %>%
                            group_by(ID) %>%
                            summarise(Mean_IV = mean(`Intraindividual Variance`)) %>%
                            arrange(Mean_IV) %>%
                            pull(ID))


fem_plot<- ggplot(DF_plot_fem, aes(x = `Intraindividual Variance`, y = ID, fill = Isotope)) +
  geom_density_ridges(alpha = 0.5, scale = 1.5) +
  theme_classic() +
  scale_fill_manual(values = c("#FF6347", "#4682B4")) +  # red for carbon, blue for nitrogen
  scale_x_continuous(limits = c(0, 4)) +
  labs(x = "Intraindividual Variance", y = "Individuals (ID)", title = "") +
  theme(text = element_text(family = "Times New Roman"))+
  theme(
    axis.title.x = element_text(size = 16), 
    axis.title.y = element_text(size = 16),  
    axis.text.x = element_text(size = 20),   
    axis.text.y = element_text(size = 20),
    legend.text = element_text(size = 16) 
  )+
  theme(legend.position="none")

fem_plot

machos <- c("Aam1",   "Aam10", "Aam11",  "Aam12",  "Aam17",  "Aam2",   "Aam3",   "Aam4",   "Aam5",   "Aam6",   "Aam7",   "Aam8",   "Aam9", "190509", "270709", "40209") 
machos1 <- subset(DF_plot, ID %in% machos)
head(machos1)

# intraindividdual variance mean for males
DF_plot_male <- machos1 %>%
  group_by(ID) %>%
  mutate(Mean_IV = mean(`Intraindividual Variance`)) %>%
  ungroup()

#Order ID by mean intraindividual variance
DF_plot_male$ID <- factor(DF_plot_male$ID, levels = DF_plot_male %>%
                       group_by(ID) %>%
                       summarise(Mean_IV = mean(`Intraindividual Variance`)) %>%
                       arrange(Mean_IV) %>%
                       pull(ID))

males_plot<- ggplot(DF_plot_male, aes(x = `Intraindividual Variance`, y = ID, fill = Isotope)) +
  geom_density_ridges(alpha = 0.5, scale = 1.5) +
  theme_classic() +
  scale_fill_manual(values = c("#FF6347", "#4682B4")) +  # red for carbon,, blue for nitrogen
  scale_x_continuous(limits = c(0, 4)) +
  labs(x = "Intraindividual Variance", y = "Individuals (males)", title = "") +
  theme(text = element_text(family = "Times New Roman"))+
  theme(
    axis.title.x = element_text(size = 16),  
    axis.title.y = element_text(size = 16),  
    axis.text.x = element_text(size = 20),   
    axis.text.y = element_text(size = 20),
    legend.text = element_text(size = 16) 
  )+
  theme(axis.title.y = element_blank())
males_plot

df_plot<-grid.arrange(fem_plot, males_plot, nrow=1)
df_plot
ggsave("individual level.tiff", df_plot, units="px", width= 8000, height = 5000, device = tiff, dpi=350)


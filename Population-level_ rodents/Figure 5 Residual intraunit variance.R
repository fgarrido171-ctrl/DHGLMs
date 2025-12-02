library(gridExtra);library(tibble)
library(ggplot2);library(gridExtra)
library(brms);library(dplyr)
library(ggridges);library(tidyr)

#Estimates of posterior distribution residual intra-individual variation (intraunit variance) for 𝛿¹³C and 𝛿15N at the population level for (a) males and (b) females of Myodes gapperi red-backed voles
model2 <- readRDS("minaki_MG.rds")
summary(model2)

# fixed effects
DF1<-as.data.frame(fixef(model2))
coef_table4 <- rownames_to_column(DF1, var = "Variable")
head(coef_table4)

# random effects
ranefs<-VarCorr(model2)
coef_random <- as.data.frame(ranefs[["site"]][["sd"]])
coef_table1 <- rownames_to_column(coef_random, var = "Variable")
coef_table1$Variable<- as.factor(coef_table1$Variable)


#plot the posterior distribution of each population’s
#predicted standard deviation (i.e. rIIV). Populationss with higher rIIV are less predictable than populations
#with lower rIIV

comunidad1<-posterior_samples(model2)
df_filtrado <- comunidad1 %>%
  select(starts_with("r_site__sigma_d"))

#In order to interpret rIIV in biological terms we backtransform rIIV by taking
#it’s original scale by taking its exponent (ref: guide for studying among-individual behavioral variation from movement data in the wild. Anne G. Hertel & Petri T. Niemelä, 2020)
community.sp<- exp(df_filtrado)
Sp.C<- community.sp[, c(1:17)]  #each individual for C 
Sp.N<- community.sp[, c(18:34)] #each individual for N 

#for Carbon

df_Carbon <- Sp.C %>%
  pivot_longer(cols = everything(), 
               names_to = "Population", 
               values_to = "Intraindividual Variance") %>%
  mutate(Population = gsub("r_site__sigma_d13C\\[|,Intercept\\]", "", Population))%>%
  arrange(Population)  # Order by population
df_Carbon<- as.data.frame(df_Carbon)
df_Carbon$Population<- as.factor(df_Carbon$Population)
str(df_Carbon)

# for Nitrogen

df_Nitrogen <- Sp.N %>%
  pivot_longer(cols = everything(), 
               names_to = "Population", 
               values_to = "Intraindividual Variance") %>%
  mutate(Population = gsub("r_site__sigma_d15N\\[|,Intercept\\]", "", Population))%>%
  arrange(Population)  # Order by population

df_N <- as.data.frame(df_Nitrogen)
df_N$Population<- as.factor(df_N$Population)
str(df_N)
alls<- rbind(df_N,df_Carbon)
Isotope <- factor(c(rep("Nitrogen", 27200), rep("Carbon", 27200)))

# create a dataframe with the variable "isotope" 
df_largo4<- data.frame(Isotope = Isotope)
DF_plot<-cbind(alls,df_largo4)

#Order ID by mean intrapopulation variance
DF_plot$Population <- factor(DF_plot$Population, levels = DF_plot %>%
                           group_by(Population) %>%
                           summarise(Mean_IV = mean(`Intraindividual Variance`)) %>%
                           arrange(Mean_IV) %>%
                           pull(Population))

pop_plot<- ggplot(DF_plot, aes(x = `Intraindividual Variance`, y = Population, fill = Isotope)) +
  geom_density_ridges(alpha = 0.5, scale = 1.5) +
  theme_classic() +
  scale_fill_manual(values = c("#FF6347", "#4682B4")) +  # Rojo para Carbono, Azul para Nitrógeno
  scale_x_continuous(limits = c(0, 3)) +  # Ajusta los límites del eje X según tus datos
  labs(x = "Intraindividual Variance", y = "Populations", title = "") +
  theme(text = element_text(family = "Times New Roman"))+
  theme(
    axis.title.x = element_text(size = 16),  # Aumentar tamaño del título del eje X
    axis.title.y = element_text(size = 16),  # Aumentar tamaño del título del eje Y
    axis.text.x = element_text(size = 20),   # Aumentar tamaño de los textos del eje X
    axis.text.y = element_text(size = 20),#Aumentar tamaño de los textos del eje Y
    legend.text = element_text(size = 16) 
  )+
  theme(legend.position="none")

pop_plot

ggsave("population level.tiff", pop_plot, units="px", width= 8000, height = 5000, device = tiff, dpi=350)


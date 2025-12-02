library(gridExtra);library(tibble)
library(ggplot2);library(gridExtra)
library(brms);library(dplyr)
library(ggridges);library(tidyr)

#Estimates of posterior distribution residual intra-individual variation (intraunit variance) for 𝛿¹³C and 𝛿15N at the community level for spp community
model3 <- readRDS("community_model.rds")
summary(model3)

# fixed effects
DF1<-as.data.frame(fixef(model3))
coef_table4 <- rownames_to_column(DF1, var = "Variable")
head(coef_table4)

#plot the posterior distribution of each spp
#predicted standard deviation (i.e. rIIV). Spp with higher rIIV are less predictable than spp
#with lower rIIV

comunidad1<-posterior_samples(model3) 

df_filtrado <- comunidad1 %>%
  select(starts_with("r_species__sigma_d"))

#In order to interpret rIIV in biological terms we backtransform rIIV by taking
#it’s original scale by taking its exponent (ref: guide for studying among-individual behavioral variation from movement data in the wild. Anne G. Hertel & Petri T. Niemelä, 2020)
community.sp<- exp(df_filtrado)
Sp.C<- community.sp[, c(1:10)]  
Sp.N<- community.sp[, c(11:20)]

# for Carbon
df_Carbon <- Sp.C %>%
  pivot_longer(cols = everything(), 
               names_to = "Sp", 
               values_to = "Intraindividual Variance") %>%
  mutate(Sp = gsub("r_species__sigma_d13c\\[|,Intercept\\]", "", Sp))%>%
  arrange(Sp)  # order by spp

df_Carbon <- as.data.frame(df_Carbon)
df_Carbon$Sp<- as.factor(df_Carbon$Sp)
str(df_Carbon)

# for Nitrogen
df_Nitrogen <- Sp.N %>%
  pivot_longer(cols = everything(), 
               names_to = "Sp", 
               values_to = "Intraindividual Variance") %>%
  mutate(Sp = gsub("r_species__sigma_d15n\\[|,Intercept\\]", "", Sp))%>%
  arrange(Sp)  # order by pop

df_N <- as.data.frame(df_Nitrogen)
df_N$Sp<- as.factor(df_N$Sp)

alls<- rbind(df_N,df_Carbon)
Isotope <- factor(c(rep("Nitrogen", 8000), rep("Carbon", 8000)))

# create a dataframe with the variable "isotope" 
df_largo4<- data.frame(Isotope = Isotope)
DF_plot<-cbind(alls,df_largo4)
str(DF_plot)

#Order ID by mean intraspp variance
DF_plot$Sp <- factor(DF_plot$Sp, levels = DF_plot %>%
                               group_by(Sp) %>%
                               summarise(Mean_IV = mean(`Intraindividual Variance`)) %>%
                               arrange(Mean_IV) %>%
                               pull(Sp))

spp_plot<- ggplot(DF_plot, aes(x = `Intraindividual Variance`, y = Sp, fill = Isotope)) +
  geom_density_ridges(alpha = 0.5, scale = 1.5) +
  theme_classic() +
  scale_fill_manual(values = c("#FF6347", "#4682B4")) +  # Rojo para Carbono, Azul para Nitrógeno
  scale_x_continuous(limits = c(0, 3)) +  # Ajusta los límites del eje X según tus datos
  labs(x = "Intraindividual Variance", y = "Species", title = "") +
  theme(text = element_text(family = "Times New Roman"))+
  theme(
    axis.title.x = element_text(size = 16),  # Aumentar tamaño del título del eje X
    axis.title.y = element_text(size = 16),  # Aumentar tamaño del título del eje Y
    axis.text.x = element_text(size = 20),   # Aumentar tamaño de los textos del eje X
    axis.text.y = element_text(size = 20),#Aumentar tamaño de los textos del eje Y
    legend.text = element_text(size = 16) 
  )+
  theme(legend.position="none")

spp_plot
ggsave("community level.tiff", spp_plot, units="px", width= 8000, height = 5000, device = tiff, dpi=350)


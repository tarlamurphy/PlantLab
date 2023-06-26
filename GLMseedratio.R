# ecological data on the pollination of flowering plants 
# load necessary libraries
library(readxl)
library(multcomp)
library(ggplot2)
library(viridis)
library(patchwork)

# load data from excel
lamium <- read_excel("poll_lim.xlsx")
summary(lamium)

# divide the dataset into the three patches
group2 <- lamium[1:16,]
group3 <- lamium[17:36,]
group4 <- lamium[37:55,]

# calculate ratio (actual number of seeds/max nr of seeds that this species can produce) to make plots easier to interpret
Ratio <- lamium$Success/4
Ratio2 <- group2$Success/4
Ratio3 <- group3$Success/4
Ratio4 <- group4$Success/4


# create a boxplot for visualization: 
boxplot_ratio <- ggplot() +
  geom_boxplot(aes(x = lamium$Test, y  = Ratio, fill = lamium$Test)) + 
  labs(title = "Ratio between Seeds and Total Seeds", 
     subtitle = "Comparison between natural state and pollen supplementation", 
     x = "Treatment", y = "Ratio", fill = "Treatment" ) +
   scale_fill_viridis(discrete = T, alpha = 0.7)

ggsave(filename = "boxplot_ratio.png", plot = boxplot_ratio, width = 5, height = 5)

# create boxplots for each of the patches
boxplot_2 <- ggplot() +
  geom_boxplot(aes(x = group2$Test, y  = Ratio2, fill = group2$Test)) + 
  labs(title = "Ratio in Patch 2", 
       subtitle = "Comparison between N and PS", 
       x = "Treatment", y = "Ratio", fill = "Treatment") +
  scale_fill_viridis(discrete = T, alpha = 0.7)

boxplot_3 <- ggplot() +
  geom_boxplot(aes(x = group3$Test, y  = Ratio3, fill = group3$Test)) + 
  labs(title = "Ratio in Patch 3", 
       subtitle = "Comparison between N and PS", 
       x = "Treatment", y = "Ratio", fill = "Treatment") +
  scale_fill_viridis(discrete = T, alpha = 0.7)

boxplot_4 <- ggplot() +
  geom_boxplot(aes(x = group4$Test, y  = Ratio4, fill = group4$Test)) + 
  labs(title = "Ratio in Patch 4", 
       subtitle = "Comparison between N and PS", 
       x = "Treatment", y = "Ratio", fill = "Treatment") +
  scale_fill_viridis(discrete = T, alpha = 0.7)

# add all boxplots together and download
patch_plots <- boxplot_2 + boxplot_3 + boxplot_4
ggsave(filename = "boxplot_patches.png", plot = patch_plots, width = 12, height = 6)

# calculate a t-test to see wether or not there are significant differences between the two treatments
t.test(Ratio[lamium$Test == "PS"], Ratio[lamium$Test == "N"])
# p-value is significant with 0.02, therfore the alternative hypothesis is accepted, the two are signficantly different

# we create a general linear model to compare seed development success/failure
model <- glm(cbind(lamium$Success, Failure) ~ lamium$Test, family = binomial, data = lamium)
summary(model)

ods <- model$deviance/model$df.residual
ods # 3.23 this is to big so adjust family to "quasibinomial"
# if >1 than the model is overdispersed so we need to adjust

# redo the model
model2 <- glm(cbind(lamium$Success, Failure) ~ lamium$Test, family = quasibinomial, data = lamium)
ods2 <- model2$deviance/model2$df.residual
ods2 
summary(model2)

# use posthoc test to define between which groups differences are found, 
install.packages("emmeans")
library(emmeans)
pairs(emmeans(model2, ~ Test)) # since we only have 2 obviously the differences are found between these

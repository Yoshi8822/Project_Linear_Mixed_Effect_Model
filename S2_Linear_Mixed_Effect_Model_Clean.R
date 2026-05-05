# Load necessary libraries
library(readr)        # read_csv()
library(dplyr)        # data manipulation
library(ggplot2)      # plotting
library(lme4)         # linear mixed models
library(emmeans)      # estimated marginal means and pairwise comparisons
library(ggprism)      # theme_prism()
library(rstatix)      # get_summary_stats()
library(ggpubr)       # ggarrange()
library(ggbeeswarm)   # geom_beeswarm()
library(performance)  # check_model()
library(lattice)      # qqmath() for random effects plot
library(effectsize)   # effectsize::eta_squared()
library(lattice)
library(sjPlot)

citation("lme4")
packageVersion("lme4")

citation("emmeans")
packageVersion("emmeans")


# Read and prepare data 
df <- read_csv("~/Documents/1 Projects/PhD folder/PhD_Projects/Study 2/Analysis_Sheet/Excel/Trial.csv")
df$Session <- factor(df$Session,
                     levels = c("TRAD","CLU_1","CLU_3"),
                     labels = c("TRAD","CLU_1","CLU_3"))
df$Rep <- factor(df$Rep,
                     levels = c("Rep1","Rep2","Rep3","Rep4","Rep5","Rep6", "Rep7", "Rep8", "Rep9"),
                     labels = c("Rep1","Rep2","Rep3","Rep4","Rep5","Rep6", "Rep7", "Rep8", "Rep9"))
df$Set <- factor(df$Set,
                 levels = c("1","2","3"),
                 labels = c("Set1","Set2","Set3"))
df <- df %>%
  mutate(Session = recode(Session,
                          "CLU_3" = "CLU-3",
                          "CLU_1" = "CLU-1"))

# Fit models
model_1 <- lmer(PV ~ 1 + Session + Rep + (1 | Subject), data = df, REML=FALSE)
model_2 <- lmer(PV ~ 1 + Session * Rep + (1 | Subject), data = df, REML=FALSE)

model_3 <- lmer(PV ~ 1 + Session * Rep * Set + (1 | Subject), data = df, REML=FALSE)
model_4 <- lmer(PV ~ 1 + Session + Rep + Set + (1 | Subject), data = df, REML=FALSE)
# Summarise everything in the table
tab_model(model_2)

# Check singularity and model diagnostics
isSingular(model_2, tol = 1e-4)
check_model(model_2)

# Model summaries
coef(model_2)
summary(model_2)
plot_model(model_2, type = "int", terms = c("Session", "Rep"))

anova(model_2)



# Assess which model best fits
anova(model_1, model_2)

anova(model_3)
summary(model_3)
# Prepare augmented dataframe for plotting fitted values
df_aug <- df %>%
  mutate(
    Rep = as.numeric(gsub("Rep", "", as.character(Rep))),  
    fitted = predict(model_2)
  )

# Plot fitted values by Subject and Session
ggplot(df_aug, aes(x = Rep, y = PV)) + 
  geom_line(aes(y = fitted, 
                group = interaction(Subject, Session), 
                linetype = Session, 
                alpha = Session), 
            linewidth = 1) +
  # Add symbols *on top of* the fitted line at each x point
  geom_point(aes(x = Rep, 
                 y = fitted, 
                 shape = Session, 
                 alpha = Session), 
             color = "black", 
             size = 2) +
  facet_wrap(~ Subject, ncol = 5) +
  labs(x = "Repetition", 
       y = "Peak Velocity (Fitted)",
       title = "Fitted Peak Velocity Across Repetition by Subject and Session") +
  scale_x_continuous(breaks = sort(unique(df_aug$Rep))) +
  
  # Set alpha levels for visual separation
  scale_alpha_manual(values = c(1.0, 0.7, 0.4)) +
  
  theme_prism() +
  theme(strip.text = element_text(size = 12),
        axis.text.y = element_text(size = 10),
        legend.position = "bottom",
        legend.title = element_blank())




##### Check assumptions #####
# Homoscedasticity
plot(fitted(model_2), resid(model_2))
abline(h = 0, col = "red")

# Normality of residuals
qqnorm(resid(model_2))
qqline(resid(model_2))

# Normality of random effects
qqmath(ranef(model_2, condVar=TRUE))

# Effect Size
effectsize::eta_squared(model_2, partial=TRUE)

# Check collinearity
check_collinearity(model_1)

# Pairwise comparisons with emmeans
emmeans(model_2, pairwise~Session, adjust = "holm")
emmeans(model_2, pairwise~Rep, adjust = "holm")
emmeans(model_2, pairwise~Session | Rep, adjust = "holm")

# Plot estimated marginal means
k <- emmip(model_2, Session ~ Rep, 
      CIs = TRUE, 
      level = 0.95, 
      dodge = 0.5,
      position = position_dodge(0.4)) +
  labs(title = "Estimated Marginal Means from Linear Mixed-Effects Model",
       x = "",
       y = "Estimated Peak Velocity") +
  theme_prism(
    base_fontface = "plain",
    base_line_size = 0.7,
    base_family = "Arial"
  ) +
  scale_x_discrete(
    # guide = guide_prism_bracket(width = 0.08),
    labels = scales::wrap_format(5)
  ) +
  # Add grayscale colors with different transparency
  scale_color_grey(start = 0, end = 0.4, aesthetics = "color", guide = "legend") +
  scale_fill_grey(start = 0, end = 0.4, aesthetics = "fill", guide = "legend") +
  guides(color = guide_legend(override.aes = list(alpha = c(1, 0.6, 0.4))),
         fill = guide_legend(override.aes = list(alpha = c(1, 0.6, 0.4)))) +
  # This adds bigger points on top of the existing ones:
  geom_point(size = 4, position = position_dodge(0.5))
k


# Additional data loading and descriptive statistics for PV decline
df1 <- read_csv("~/Documents/1 Projects/PhD folder/PhD_Projects/Study 2/Analysis_Sheet/Excel/Master_Sheet_PVdecline.csv")

#Descriptive Table
Descriptive_data <- df1 %>% 
  select(Subject, Session, Set, Rep, PV) %>% 
  arrange(Session, Set, Rep) 

write_excel_csv(Descriptive_data, file = 'Desciptive_Data.csv')

# Session - CLU1
df2 <- df1 %>% 
  group_by(Subject, Session, Rep) %>% 
  get_summary_stats(PV, type = "mean_sd") %>% 
  dplyr::select(Subject, Session, Rep, mean, sd)　%>% 
  dplyr::filter(Session == "CLU_1")

# df2$Rep <- factor(df2$Rep, levels = c("1", "2", "3", "4", "5", "6", "7", "8",
#                                       "9"))

ggp2_df2 <- ggplot(df2, aes(x = Rep, y = mean)) 

p <- ggp2_df2 + ggbeeswarm::geom_beeswarm(
  aes(x = Rep, y = mean), 
  dodge.width = 1, 
  shape = 1,
  size = 2,
  show.legend = FALSE,
  cex = 0.6) +
  geom_line(
    aes(group = interaction(Subject, Session)),
    alpha = 0.5) +
  stat_summary(
    geom = "crossbar",
    aes(fill = Session),
    fun = mean,
    position = position_dodge(0.1),
    colour = "black",
    size = 0.9, 
    width = 0.4,
    show.legend = FALSE,
    alpha = 0.2 
  ) +
  theme_prism(
    base_fontface = "plain", 
    base_line_size = 0.7, 
    base_family = "Arial",
  ) + 
  scale_x_discrete(
    # guide = guide_prism_bracket(width = 0.08), 
    labels = scales::wrap_format(5)
  ) + 
  scale_y_continuous(limits = c(-20, 8)) +# Set y-axis limits manually
  theme(
    legend.position = "",
  ) + 
  labs(title = "PV change during CLU-1", x = "", y = "Percentage Velocity Decline (%)")

p


# Session - TRAD
df3 <- df1 %>% 
  group_by(Subject, Session, Rep) %>% 
  get_summary_stats(PV, type = "mean_sd") %>% 
  dplyr::select(Subject, Session,Rep, mean, sd)　%>% 
  dplyr::filter(Session == "TRAD")

# df3$Rep <- factor(df3$Rep, levels = c("1", "2", "3", "4", "5", "6", "7", "8",
#                                       "9"))

ggp3_df3 <- ggplot(df3, aes(x = Rep, y = mean)) 



p2 <- ggp3_df3 + ggbeeswarm::geom_beeswarm(
  aes(x = Rep, y = mean), 
  dodge.width = 1, 
  shape = 1,
  size = 2,
  show.legend = FALSE,
  cex = 0.6
) + 
  geom_line(
    aes(group = interaction(Subject, Session)),
    alpha = 0.5
  ) + 
  stat_summary(
    geom = "crossbar",
    aes(fill = Session),
    fun = mean,
    position = position_dodge(0.1),
    colour = "black",
    size = 0.9, 
    width = 0.4,
    show.legend = FALSE,
    alpha = 0.2 
  ) +
  scale_y_continuous(limits = c(-20, 8)# Set y-axis limits manually
  ) +
  theme_prism(
    base_fontface = "plain", 
    base_line_size = 0.7, 
    base_family = "Arial",
  ) +
  scale_x_discrete(
    # guide = guide_prism_bracket(width = 0.08), 
    labels = scales::wrap_format(5)
  ) + theme(
    legend.position = "",
  ) + labs(title = "PV change during TRAD", x = "", y = "Percentage Velocity Decline (%)")

p2




# Session - CLU3
df5 <- df1 %>% 
  group_by(Subject, Session, Rep) %>% 
  get_summary_stats(PV, type = "mean_sd") %>% 
  dplyr::select(Subject, Session,Rep, mean, sd)　%>% 
  dplyr::filter(Session == "CLU_3")

# df5$Rep <- factor(df5$Rep, levels = c("1", "2", "3", "4", "5", "6", "7", "8",
#                                       "9"))

ggp3_df5 <- ggplot(df5, aes(x = Rep, y = mean)) 



p3 <- ggp3_df5 + ggbeeswarm::geom_beeswarm(
  aes(x = Rep, y = mean), 
  dodge.width = 1, 
  shape = 1,
  size = 2,
  show.legend = FALSE,
  cex = 0.6
) + 
  geom_line(
    aes(group = interaction(Subject, Session)),
    alpha = 0.5
  ) + 
  stat_summary(
    geom = "crossbar",
    aes(fill = Session),
    fun = mean,
    position = position_dodge(0.1),
    colour = "black",
    size = 0.9, 
    width = 0.4,
    show.legend = FALSE,
    alpha = 0.2 
  ) +
  scale_y_continuous(limits = c(-20, 8)# Set y-axis limits manually
  ) +
  theme_prism(
    base_fontface = "plain", 
    base_line_size = 0.7, 
    base_family = "Arial",
  ) +
  scale_x_discrete(
    # guide = guide_prism_bracket(width = 0.08), 
    labels = scales::wrap_format(5)
  ) + theme(
    legend.position = "",
  ) + labs(title = "PV change during CLU-3", x = "", y = "Percentage Velocity Decline (%)")

p3


#  Combine these 
ggarrange(k, p2, p , p3,
          labels = c("A", "B", "C", "D"),
          nrow = 2, ncol = 2)




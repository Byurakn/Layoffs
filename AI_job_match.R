library(ggplot2)
library(lme4)
library(lmerTest)
library(dplyr)
library(emmeans)

d <- read.csv("AI_recruiter_applicant_data.csv")

# some raw plot
p <- ggplot(d, aes(x = LLM, y = Score, color = Role)) + 
  geom_point(position = "jitter") + geom_boxplot() +
  theme_classic()
p

# model with interaction
m <- lmer(data=d, Score ~ LLM*Role + (1|Job_ID))
summary(m)
plot(m)
qqnorm(resid(m))
qqline(resid(m))


# model without interaction
m2 <- lmer(data=d, Score ~ LLM+Role + (1|Job_ID))
summary(m2)
plot(m2)
qqnorm(resid(m2))
qqline(resid(m2))

# model comparison
anova(m, m2)

# no big difference between the two models, so sticking to the one with interaction
# for theoretical reasons

# Predicted means with CIs
pred_data <- expand.grid(
  LLM  = unique(d$LLM),
  Role = unique(d$Role)
)
pred_data$Score <- predict(m, newdata = pred_data, re.form = NA)

emm <- emmeans(m, ~ LLM * Role)
pred_data <- as.data.frame(emm)
names(pred_data)[names(pred_data) == "emmean"] <- "Score"
names(pred_data)[names(pred_data) == "lower.CL"] <- "lower"
names(pred_data)[names(pred_data) == "upper.CL"] <- "upper"
# Plot
ggplot(pred_data, aes(x = Role, y = Score, color = LLM, group = LLM)) +
  geom_line(linewidth = 1.2, position = position_dodge(0.3)) +
  geom_point(size = 3.5, position = position_dodge(0.3)) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.15, linewidth = 0.9,
                position = position_dodge(0.3)) +
  geom_jitter(data = d, aes(x = Role, y = Score, color = LLM),
              alpha = 0.15, width = 0.15, height = 0,
              inherit.aes = FALSE) +
  scale_color_manual(values = c("GPT" = "blue", "Claude" = "red"))+
  labs(
    title    = "Predicted Scores by LLM and Role",
    #subtitle = "Error bars = 95% CI",
    x        = "Role",
    y        = "Score",
    color    = "LLM"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title      = element_text(face = "bold"),
    legend.position = "top",
    panel.grid.minor = element_blank()
  )


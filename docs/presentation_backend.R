library(here)

i_am("output/presentation/presentation_backend.R")

pic_dir = here("output", "presentation", "pictures")

# PLOT TEMPLATE -----------------------------------------------------------

library(extrafont)
library(ggplot2)
library(ggbrace)

ylim = c(0.8, 2)

theme_MH <- function(){
  theme(text = element_text(family = "CMU Serif",
                            size = 14),
        legend.position = "bottom") +
    theme_minimal()
}


# HOW TO SAVE PLOTS -------------------------------------------------------
# 
# plot <- ggplot(data = mtcars, mapping = aes(x = mpg, y = cyl)) +
#   geom_point() +
#   theme_MH()
# 
# ## FULL PAGE PLOT
# ggsave(plot,
#        device = svg,
#        filename = "pictures/plot_full.svg",
#        height = 6,
#        width = 12)
# 
# # HALF PAGE PLOT
# ggsave(plot,
#        device = svg,
#        filename = "pictures/plot_half.svg",
#        height = 5,
#        width = 6)


# STANDARD FUNCTIONS ------------------------------------------------------

library(tidyverse)


datafun = function(marathon_effect, year_effect, bombing_effect, slope, short, base_intercept = 1){
  tibble(D_i = -35:35,
         Year_i = 0,
         Year = "2012",
         x = seq(from = 0, to = length(D_i)-1)) %>% 
    bind_rows(tibble(D_i = -35:35,
                     Year_i = 1,
                     Year = "2013",
                     x = seq(from = 0, to = length(D_i)-1)
    )) %>% 
    mutate(Post_Marathon_i = if_else(D_i < 0, 0, 1),
           Post_Marathon = if_else(D_i < 0, "Pre Marathon", "Post Marathon"),
           Bombing_i = Year_i * Post_Marathon_i,
           W_i = base_intercept + x/max(x) * slope   
           +   marathon_effect  * Post_Marathon_i 
           +   year_effect  * Year_i
           +   ifelse(Bombing_i == 1, 
                      shortfun(D_i, bombing_effect, short),
                      0)
    )
}

shortfun = function(D_i, bombing_effect, short){
  if(short){
    ifelse(D_i < 0,
           0, D_i/max(D_i)*-bombing_effect + bombing_effect)
    
  } else {
    bombing_effect
  }
}


# Simple RDD --------------------------------------------------------------

data_1 = datafun(marathon_effect = 0,
                 year_effect = 0,
                 bombing_effect = -0.2,
                 slope = 1,
                 short = F)

# Plot without Regression line
plot_RDD_1 = data_1 %>%
  filter(Year == "2013") %>%
  ggplot(aes(x = x, y = W_i)) +
  geom_point(size = 1.75) +
  labs(x = "x_i",
       y = "y_i") +
  theme_MH() +
  lims(y = ylim)

# Save Plot
ggsave(plot_RDD_1,
       filename = here(pic_dir, "plot_RDD_1.svg"),
       height = 6,
       width = 6)

# Plot with RDD line
RDD_fit_1 = data_1 %>% 
  filter(Year == "2013") %>% 
  transmute(x,
            Post_Marathon_i,
            RDD = lm(W_i ~ Bombing_i + x)$fitted.values)


plot_RDD_2 = data_1 %>%  
  filter(Year == "2013") %>% 
  ggplot(aes(x = x, y = W_i)) +
  geom_line(data = RDD_fit_1,
            inherit.aes = F,
            size = 1,
            aes(x = x, y = RDD,
                group = Post_Marathon_i),
            color = "red") +
  
  geom_point(size = 1.75) +
  labs(x = "x_i",
       y = "y_i") +
  geom_segment(x = 34.7                  , xend = 34.7,
               y = RDD_fit_1$RDD[35]+0.01, yend = RDD_fit_1$RDD[36],
               color = "red",
               size = 1.75,
               alpha = 0.7) +
  geom_text(x = 36, y = RDD_fit_1$RDD[36]+0.1,
            label = expression(beta)) +
  theme_MH() +
  lims(y = ylim)

# Save Plot
ggsave(plot_RDD_2,
       filename = here(pic_dir, "plot_RDD_2.svg"),
       height = 6,
       width = 6)



# LLR RDD -----------------------------------------------------------------

data_2 = datafun(marathon_effect = 0,
                 year_effect = 0,
                 bombing_effect = -0.2,
                 slope = 1,
                 short = T)

# Plot without Regression line
plot_LLR_RDD_1 = data_2 %>%  
  filter(Year == "2013") %>% 
  ggplot(aes(x = D_i, y = W_i)) +
  geom_point(size = 1.75) +
  labs(x = "x_i - c",
       y = "y_i") +
  theme_MH() +
  lims(y = ylim)

# Save Plot
ggsave(plot_LLR_RDD_1,
       filename = here(pic_dir, "plot_LLR_RDD_1.svg"),
       height = 6,
       width = 6)

# Plot with RDD line
RDD_fit_2 = data_2 %>% 
  filter(Year == "2013") %>% 
  transmute(x,
            Post_Marathon_i,
            D_i,
            RDD = lm(W_i ~ Bombing_i + x)$fitted.values)

# Plot with not fitting RDD line
plot_LLR_RDD_2 = data_2 %>%  
  filter(Year == "2013") %>% 
  ggplot(aes(x = x, y = W_i)) +
  geom_line(data = RDD_fit_2,
            inherit.aes = F,
            size = 1,
            aes(x = x, y = RDD,
                group = Post_Marathon_i),
            color = "red") +
  
  geom_point(size = 1.75) +
  labs(x = "x_i",
       y = "y_i") +
  geom_segment(x = 35                    , xend = 35,
               y = RDD_fit_2$RDD[35]+0.01, yend = RDD_fit_2$RDD[36],
               color = "red",
               size = 1.5,
               alpha = 0.7) +
  geom_text(x = 36.5, y = RDD_fit_2$RDD[36]+0.1,
            label = expression(beta),
            size = 8) +
  theme_MH() +
  lims(y = ylim)

# Save Plot
ggsave(plot_LLR_RDD_2,
       filename = here(pic_dir, "plot_LLR_RDD_2.svg"),
       height = 6,
       width = 6)

# Use LLR RDD
LLR_RDD_fit = data_2 %>% 
  filter(Year == "2013") %>% 
  transmute(x,
            Post_Marathon_i,
            D_i,
            RDD = lm(W_i ~ Post_Marathon_i + I(D_i*Post_Marathon_i) + I(D_i*(1-Post_Marathon_i)),
                     data = .)$fitted.values)

# Splitted Regressions plot
plot_LLR_RDD_3 = data_2 %>%  
  filter(Year == "2013") %>% 
  ggplot(aes(x = D_i, y = W_i)) +
  geom_line(data = LLR_RDD_fit[1:35,],
            inherit.aes = F,
            size = 1,
            aes(x = D_i, y = RDD,
                group = Post_Marathon_i),
            colour = "darkgreen") +
  
  geom_point(size = 1.75) +
  labs(x = "x_i - c",
       y = "y_i") +
  geom_segment(x = -1               , xend = -1,
               y = ylim[1], yend = LLR_RDD_fit$RDD[35],
               color = "darkgreen",
               size = 1.5,
               alpha = 0.7) +
  geom_text(x = -4, y = ylim[1]+0.3,
            label = expression(alpha[L]),
            size = 8) +
  theme_MH() +
  lims(y = ylim)

# Save Plot
ggsave(plot_LLR_RDD_3,
       filename = here(pic_dir, "plot_LLR_RDD_3.svg"),
       height = 6,
       width = 6)

plot_LLR_RDD_4 = plot_LLR_RDD_3 +
  geom_line(data = LLR_RDD_fit[36:72,],
            inherit.aes = F,
            size = 1,
            aes(x = D_i, y = RDD,
                group = Post_Marathon_i),
            colour = "blue") +
  geom_segment(x = 0               , xend = 0,
               y = ylim[1], yend = LLR_RDD_fit$RDD[36],
               color = "blue",
               size = 1.5,
               alpha = 0.7) +
  geom_text(x = 3, y = ylim[1]+0.25,
            label = expression(alpha[R]),
            size = 8)

# Save Plot
ggsave(plot_LLR_RDD_4,
       filename = here(pic_dir, "plot_LLR_RDD_4.svg"),
       height = 6,
       width = 6)


plot_LLR_RDD_5 = plot_LLR_RDD_4 +
  geom_segment(x = 0                     , xend = 0,
               y = LLR_RDD_fit$RDD[35]+0.01, yend = LLR_RDD_fit$RDD[36],
               color = "red",
               size = 1.5,
               alpha = 0.7) +
  geom_text(x = 1.5, y = LLR_RDD_fit$RDD[36]+0.1,
            label = expression(beta),
            size = 8)

# Save Plot
ggsave(plot_LLR_RDD_5,
       filename = here(pic_dir, "plot_LLR_RDD_5.svg"),
       height = 6,
       width = 6)

# Plot with fitting LLR RDD line
plot_LLR_RDD_4 = data_2 %>%  
  filter(Year == "2013") %>% 
  ggplot(aes(x = x, y = W_i)) +
  geom_line(data = LLR_RDD_fit,
            inherit.aes = F,
            size = 1,
            aes(x = x, y = RDD,
                group = Post_Marathon_i),
            colour = "red") +
  
  geom_point(size = 1.75) +
  labs(x = "x_i",
       y = "y_i") +
  geom_segment(x = 35               , xend = 35,
               y = LLR_RDD_fit$RDD[35]+0.01, yend = LLR_RDD_fit$RDD[36],
               color = "red",
               size = 1.5,
               alpha = 0.7) +
  geom_text(x = 36, y = LLR_RDD_fit$RDD[36]+0.1,
            label = expression(beta)) +
  theme_MH() +
  lims(y = ylim)


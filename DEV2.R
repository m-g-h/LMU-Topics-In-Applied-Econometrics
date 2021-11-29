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



# Global Stuff ------------------------------------------------------------

ylim = c(0.8, 2)

# Without Year and Marathon effects ---------------------------------------

data_1 = datafun(marathon_effect = 0,
                 year_effect =  0,
                 bombing_effect =  -0.2,
                 slope =  1,
                 short = F) 

data_1 %>%  
  mutate(W_i = W_i+0.005*Year_i) %>% 
  ggplot(aes(x = D_i, y = W_i, color = Year)) +
  geom_point() +
  labs(title = "No Marathon Effect - Only Bombing") +
  lims(y = ylim)

# The Linear RDD captures the Bombing effect perfectly

RDD_fun = function(data){
  data %>% 
    lm(W_i ~ Bombing_i + D_i,
       data = .) %>% 
    summary()
}

data_1 %>% 
  filter(Year == "2013") %>% 
  RDD_fun()

RDD_fit = data_1 %>% 
  filter(Year == "2013") %>% 
  transmute(D_i,
            RDD = lm(W_i ~ Bombing_i + D_i)$fitted.values)

data_1 %>%  
  mutate(W_i = W_i+0.005*Year_i) %>% 
  ggplot(aes(x = D_i, y = W_i, color = Year)) +
  geom_point() +
  geom_line(data = RDD_fit,
            inherit.aes = F,
            aes(x = D_i, y = RDD)) +
  labs(title = "No Marathon Effect - Only Bombing") +
  lims(y = ylim)

# With Marathon Effect ----------------------------------------------------

data_2 = datafun(marathon_effect = 0.2,
                 year_effect =  0,
                 bombing_effect =  -0.2,
                 slope =  1,
                 short = F)

data_2 %>%  
  mutate(W_i = W_i+0.005*Year_i) %>% 
  ggplot(aes(x = D_i, y = W_i, color = Year)) +
  geom_point() +
  labs(title = "Bombing is offset by Marathon") +
  lims(y = ylim)

# The RDD captures no Bombing effect
data_2 %>% 
  filter(Year == "2013") %>% 
  RDD_fun()


RDD_fit = data_2 %>% 
  filter(Year == "2013") %>% 
  transmute(D_i,
            RDD = lm(W_i ~ Bombing_i + D_i)$fitted.values)

data_2 %>%  
  mutate(W_i = W_i+0.0005*Year_i) %>% 
  ggplot(aes(x = D_i, y = W_i, color = Year)) +
  geom_point() +
  geom_line(data = RDD_fit,
            inherit.aes = F,
            aes(x = D_i, y = RDD)) +
  labs(title = "Bombing is offset by Marathon") +
  lims(y = ylim)


# Use Diff-in-Diffs to control for Marathon Effect ------------------------

DiD_fun = function(data){
  data %>% 
    lm(W_i ~ Bombing_i + Year_i + Post_Marathon_i,
       data = .) %>% 
    summary()
}

# The Diff-in-Diffs finds the Marathon Effect both with and without a slope
data_2 %>% 
  DiD_fun()

datafun(marathon_effect = 0.2,
        year_effect =  0,
        bombing_effect =  -0.2,
        slope =  0,
        short = F) %>% 
  DiD_fun()


# Problem: Short-Lived Effects --------------------------------------------

data_3 = datafun(marathon_effect = 0,
                 year_effect = 0,
                 bombing_effect =  -0.2,
                 slope = 1,
                 short =  T) 

# Bombing effect now declines over time
data_3 %>%  
  mutate(W_i = W_i+0.0001*Year_i) %>% 
  ggplot(aes(x = D_i, y = W_i, color = Year)) +
  geom_point() +
  labs(title = "No Marathon Effect - Only Bombing - Short Effect Length") +
  lims(y = ylim)

# Diff in Diff finds no precise effect
data_3 %>% 
  DiD_fun()

# RDD finds nothing
data_3 %>% 
  filter(Year == "2013") %>% 
  RDD_fun()

RDD_fit = data_3 %>% 
  filter(Year == "2013") %>% 
  transmute(D_i,
            RDD = lm(W_i ~ Bombing_i + D_i)$fitted.values)

data_3 %>%  
  mutate(W_i = W_i+0.0001*Year_i) %>% 
  ggplot(aes(x = D_i, y = W_i, color = Year)) +
  geom_point() +
  geom_line(data = RDD_fit,
            inherit.aes = F,
            aes(x = D_i, y = RDD)) +
  labs(title = "No Marathon Effect - Only Bombing - Normal RDD") +
  lims(y = ylim)


# Allowing for different slopes in RDD -------------------------------------------

RDD_LLR_fun = function(data){
  data %>% 
    lm(W_i ~ Post_Marathon_i + I(D_i*Post_Marathon_i) + I(D_i*(1-Post_Marathon_i)),
       data = .) %>% 
    summary()
}

data_3 %>% 
  filter(Year == "2013") %>% 
  RDD_LLR_fun()

RDD_fit = data_3 %>% 
  filter(Year == "2013") %>% 
  transmute(D_i,
            RDD = lm(W_i ~ Bombing_i + I(D_i*Post_Marathon_i) + I(D_i*(1-Post_Marathon_i)))$fitted.values)

data_3 %>%  
  mutate(W_i = W_i+0.0001*Year_i) %>% 
  ggplot(aes(x = D_i, y = W_i, color = Year)) +
  geom_point() +
  geom_line(data = RDD_fit,
            inherit.aes = F,
            aes(x = D_i, y = RDD)) +
  labs(title = "No Marathon Effect - Only Bombing - LLR RDD") +
  lims(y = ylim)

# Problem: Allowing for different slopes in RDD doesnt fix Marathon Effect ----

data_4 = datafun(marathon_effect = 0.2,
                 year_effect = 0,
                 bombing_effect =  -0.2,
                 slope = 1,
                 short =  T) 

# RDD LLR doesnt find the Bombing Effect
data_4 %>% 
  filter(Year == "2013") %>% 
  RDD_LLR_fun()

RDD_fit = data_4 %>% 
  filter(Year == "2013") %>% 
  transmute(D_i,
            RDD = lm(W_i ~ Bombing_i + I(D_i*Post_Marathon_i) + I(D_i*(1-Post_Marathon_i)))$fitted.values)

data_4 %>%  
  mutate(W_i = W_i+0.0001*Year_i) %>% 
  ggplot(aes(x = D_i, y = W_i, color = Year)) +
  geom_point() +
  geom_line(data = RDD_fit,
            inherit.aes = F,
            aes(x = D_i, y = RDD)) +
  labs(title = "No Marathon Effect - Only Bombing - LLR RDD") +
  lims(y = ylim)

# Solution: RDD and DiD

# Define RDD combined with Diff in Diffs
RDD_DiD_fun = function(data){
  data %>% 
    lm(W_i ~ Bombing_i + Year_i + Post_Marathon_i 
       + I(D_i*(1-Post_Marathon_i)*Year_i) 
       + I(D_i*Post_Marathon_i*Year_i) 
       + I(D_i*(1-Post_Marathon_i)*(1-Year_i))
       + I(D_i*Post_Marathon_i*(1-Year_i)),
       data = .) %>% 
    summary()
}

data_4 %>% 
  RDD_DiD_fun()

RDD_fit = data_4 %>% 
  transmute(D_i,
            Year,
            RDD = lm(W_i ~ Bombing_i + Year_i + Post_Marathon_i 
                     + I(D_i*(1-Post_Marathon_i)*Year_i) 
                     + I(D_i*Post_Marathon_i*Year_i) 
                     + I(D_i*(1-Post_Marathon_i))
                     + I(D_i*Post_Marathon_i),
                     data = .)$fitted.values
  )

data_4 %>%  
  mutate(W_i = W_i+0.005*Year_i) %>% 
  ggplot(aes(x = D_i, y = W_i, color = Year)) +
  geom_point() +
  geom_line(data = RDD_fit,
            inherit.aes = F,
            aes(x = D_i, y = RDD, color = Year)) +
  labs(title = "No Marathon Effect - Only Bombing - LLR RDD") +
  lims(y = ylim)

data_4 %>% 
  filter(Year == "2012") %>% 
  RDD_LLR_fun()

data_4 %>% 
  filter(Year == "2013") %>% 
  RDD_LLR_fun()

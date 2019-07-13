
# Install and open packages -----------------------------------------------
install.packages("tidyverse")
library(tidyverse)

###1. Importing: **readr** package
  
#Importing `got_char`

got_char <- read_csv("./got_char.csv")
head(got_char,3)
dim(got_char)

#Importing `got_houses`

got_houses <- read_csv("./got_houses.csv")
head(got_houses, 3)
dim(got_houses)


###2. Merging the two datasets
  
got_complete <- got_char %>% 
  left_join(got_houses, by = c("actor" = "name"))

head(got_complete, 3)

### 1. Building the Top 10

###**Mutate** function
  
got_complete_rank  <- got_complete %>%
  mutate(total = season_1 + season_2 + season_3 +
           season_4 + season_5 + season_6 + season_7 )

### Conditional terms using **Mutate**

got_complete_rank <- got_complete_rank %>% 
  mutate(rank = case_when(
    total >= 200 ~ "Main cast",
    total <200 & total >= 50 ~ "Supporting cast",
    total <50 ~ "Extra"))

got_complete_rank  <- got_complete_rank %>%
  mutate(house_a = ifelse(is.na(house_a), "Other", house_a))

### **Arrange** function

#Ascending
got_complete_rank <- got_complete_rank %>% 
  arrange(total)

#Descending

got_complete_rank <- got_complete_rank %>% 
  arrange(desc(total))

### **Select** function
  
top_ten<- got_complete_rank%>%
  select(actor, total, house_a) 

head(top_ten)

###**Rename** function
  
top_ten<- top_ten %>% 
  rename(Character = actor,
         House = house_a,
         `Total acting time` = total)

###**Slice** function

top_ten <- top_ten %>% 
  slice(1:10)

top_ten

###2. How is the gender distribution across houses?
  
# Let's start with a basic bar plot

got_complete_rank %>% 
    ggplot(aes(house_a)) +
    geom_bar()

# Now let's add add gender
  
got_complete_rank %>% 
      ggplot(aes(house_a, fill = gender)) +
      geom_bar()

# Let's change gender to the proper format

got_complete_rank %>% 
      mutate(gender = as.factor(gender)) %>% 
      ggplot(aes(house_a, fill = gender)) +
      geom_bar()

# Exclude NA's in gender
got_complete_rank %>% 
      filter(!is.na(gender)) %>% 
      mutate(gender = as.factor(gender)) %>% 
      ggplot(aes(house_a, fill = gender)) +
      geom_bar()

# Flip the coords
got_complete_rank %>% 
      mutate(gender = as.factor(gender)) %>% 
      ggplot(aes(house_a, fill= gender)) +
      geom_bar() +
      coord_flip()

##Details count :)
got_complete_rank %>% 
      filter(!is.na(gender)) %>% 
      mutate(gender = as.factor(gender)) %>% 
      ggplot(aes(house_a, fill= gender)) +
      geom_bar() +
      coord_flip() +
      labs(title = "Distribution of gender across the houses",
           x = "Number of characters",
           y = "House",
           fill = "Gender") +
      theme_minimal() 

#3. How is the protagonism distribution across houses?

got_complete_rank %>% 
  ggplot(aes(house_a, fill= rank)) +
  geom_bar() +
  coord_flip() +
  labs(title = "Protagonism across the houses",
       x = "Number of characters",
       y = "House",
       fill = "Protagonism") +
  theme_minimal() 

#4.How is the distribution of total time per gender
  
got_complete_rank%>% 
  filter(!is.na(gender)) %>% 
  mutate(gender = ifelse(gender == 1, "Male", "Female")) %>% 
  ggplot(aes(total)) +
  geom_histogram() +
  facet_wrap(.~gender) +
  labs(title = "Total time of acting across the 7 seasons, by gender",
       x = "Time (min)",
       y = "Number of characters") +
  theme_minimal()

###5. How was the evolution of the protagonists across seasons?

# Gather (from wide to long)

got_long <- got_complete %>% 
  select(-house_b) %>% 
  gather(variable, time, season_1:season_7, -c(actor, house_a, gender))

# Split the variable, and arrange

got_long <- got_long %>% 
  separate(variable, c("variable", "season"))

# A few more changes
  
got_long <- got_long %>% 
  filter(time >0) %>% 
  select(-c(variable)) %>% 
  arrange(actor)

# **Group_by** and **Summarize**
  
got_long %>% 
  group_by(actor) %>% 
  summarize(total = sum(time),
            max = max(time)) %>% 
  head()

#**Group** -> **Mutate** -> **Ungroup**
  
got_long <- got_long %>% 
  group_by(actor) %>% 
  mutate (total = sum(time)) %>% 
  ungroup ()

#Back to the graph
  
#Basic plot
got_long %>% 
      ggplot(aes(season, time))+
      geom_point() + 
      geom_line()

#Basic plot by actors
    got_long %>% 
      ggplot(aes(season, time, group = actor))+
      geom_point() + 
      geom_line()

#Filter the top ten (>130min)
got_long %>% 
      filter(total >130) %>% 
      ggplot(aes(season, time, group = actor))+
      geom_point() + 
      geom_line()

###Add a color!
got_long %>% 
      filter(total >130) %>% 
      ggplot(aes(season, time, group = actor, color = actor))+
      geom_point() + 
      geom_line()

# Details
  
got_long %>% 
  filter(total >130) %>% 
  ggplot(aes(season, time, group = actor, color = actor)) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  labs(title = "Evolution of the protagonists across seasons",
       x = "Season",
       y = "Total time (min)",
       color = "Protagonist") +
  theme_minimal()


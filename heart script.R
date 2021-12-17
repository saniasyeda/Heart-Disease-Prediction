download.packages(tidyverse)
heart <- read_csv("Case Study/Capstone Project/heart.csv")
heart2 <- read_csv("Case Study/Capstone Project/heart.csv")
setwd("Case Study/Capstone Project/heart.csv")
dir("Data", full.names = T)

head(heart)
View(heart)
tail(heart)
glimpse(heart)
ncol(heart)
nrow(heart)
colnames(heart)
summary(heart)
heart2 %>%
  
  mutate_if(is.character, as.factor)%>%
  dplyr::select(sex, fbs, exang, cp, restecg, slope, ca, thal, target, everything())
mutate(sex = if_else(sex == 1, "MALE", "FEMALE"),
       fbs = if_else(fbs == 1, ">120", "<=120"),
       exang = if_else(exang == 1, "YES" ,"NO"),
       cp = if_else(cp == 1, "ATYPICAL ANGINA",
                    if_else(cp == 2, "NON-ANGINAL PAIN", "ASYMPTOMATIC")),
       restecg = if_else(restecg == 0, "NORMAL",
                         if_else(restecg == 1, "ABNORMALITY", "PROBABLE OR DEFINITE")),
       slope = as.factor(slope),
       ca = as.factor(ca),
       thal = as.factor(thal),
       target = if_else(target == 1, "YES", "NO")
) %>% 
  mutate_if(is.character, as.factor) %>% 
  dplyr::select(target, sex, fbs, exang, cp, restecg, slope, ca, thal, everything())

Data Visualization

view(heart2)
# Bar Plot for target (heart disease)

ggplot(heart2, aes(x=heart2$target, fill=heart2$target))+
  geom_bar(color = 'Orange')+
  xlab("Heart Disease")+
  ylab("Count")+
  ggtitle("Presence & Absence of Heart Disease")+
  scale_fill_discrete(name= 'Heart Disease', labels =c("Absence","Presence"))

#Frequency of getting Heart Disease 
prop.table(table(heart$target)) 
45% dont get the Heart Disease 
54% do get Heart Disease 

Count the frequency of the Values of age
heart2 %>%
  group_by(age) %>%
  count() %>%
  filter(n>10) %>%
  ggplot()+
  geom_col(aes(age, n), fill = 'green')+
  ggtitle("Age Analysis")+
  xlab("Age")+
  ylab("Agecount")
Reflection: The data in Plot shows that people over 
45 (age) tends to get more heart disease. 
  

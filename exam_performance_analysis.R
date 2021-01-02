library(tidyverse)
library(ggplot2)
library(dplyr)
#DATA VISUALIZATIONS
student_data <- read_csv("StudentsPerformance.csv", 
                         col_types = cols(
                           gender = col_factor(),
                           `parental level of education` = col_factor(),
                           `test preparation course` = col_factor(),
                           lunch = col_factor()
                         ))
student_data
View(student_data)

student_data<- student_data  %>% mutate(`Average Score`= (student_data$`math score`+student_data$`reading score`+student_data$`writing score`)/3)
new_cols <- c("Gender","Race/Ethnicity", "Parental Level of Education", "Lunch","Test Preparation Course","Math Score", "Reading Score", "Writing Score", "Average Score") 
colnames(student_data) <- new_cols

student_data_w_scores <- student_data %>% mutate(n = row_number())
student_data_w_scores <- student_data %>% gather(`Math Score`:`Writing Score`, key = 'Subject', value = "Score")
student_data_w_scores

#Parental Level of Education
ggplot(data = student_data, aes(`Parental Level of Education`, fill=`Parental Level of Education`)) + geom_bar() + geom_text(stat='count', aes(label=..count..), vjust=-0.5) + ylab("Student Count")
ggplot(student_data, aes(Gender, fill=Gender)) + geom_bar() +  geom_text(stat='count', aes(label=..count..), vjust=2)  + facet_wrap(vars(`Parental Level of Education`)) + labs(y='Student Count') + theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank()) 

educ_order <- c("master's degree","bachelor's degree", "associate's degree","some college", "high school", "some high school")
ggplot(student_data_w_scores, aes(y = Score, x = fct_relevel(`Parental Level of Education`,educ_order), fill=fct_relevel(`Parental Level of Education`, educ_order))) +
  geom_boxplot() + 
  stat_summary(fun=mean, geom="point") +
  facet_grid(. ~ Subject) + labs(x="Parental Level of Education") +
  scale_x_discrete(guide = guide_axis(angle = 80)) + 
  guides(fill=guide_legend(title="Parental Level of Education"))

#Gender
ggplot(data = student_data, aes(Gender, fill=`Gender`)) + geom_bar(width = 0.85) + geom_text(stat='count', aes(label=..count..), vjust=2)+ labs(y='Student Count') 
men_math_score <- filter(student_data, Gender=="male") %>% select(`Math Score`)
fem_math_score <- filter(student_data, Gender=="female") %>% select(`Math Score`)
men_math_score;fem_math_score
density(men_math_score$`Math Score`)
density(fem_math_score$`Math Score`)

par(mfrow=c(2,1))
plot(density(men_math_score$`Math Score`), main="Density for Mens Math Scores")
plot(density(fem_math_score$`Math Score`), main="Density for Females Math Scores")

require(gridExtra)
pl1 <- ggplot(men_math_score, aes(`Math Score`)) + geom_histogram(fill='orange', color='black') + labs(title='Histogram of Mens Math Scores', x = 'Math Score', y='Student Count') 
pl2 <- ggplot(fem_math_score, aes(`Math Score`)) + geom_histogram(fill='lightblue', color='black') + labs(title='Histogram of Females Math Scores', x = 'Math Score', y='Student Count')
grid.arrange(pl1,pl2, nrow=2)
?facet_grid

mens_scores <- student_data_w_scores %>% filter(Gender == 'male')
females_scores <- student_data_w_scores %>% filter(Gender == 'female')

require(gridExtra)
p1 <- ggplot(mens_scores, aes(Score)) + geom_histogram(fill="orange",color='black') + facet_grid(. ~ Subject) + labs(title="Mens Scores Distribution", y="Student Count")
p2 <- ggplot(females_scores, aes(Score)) + geom_histogram(fill="lightblue",color='black') + facet_grid(. ~ Subject) + labs(title = "Womens Scores Distribution", y="Student Count")
grid.arrange(p1,p2,nrow = 2)

gc()

ggplot(student_data_w_scores, aes(x=Gender, y=Score, fill=Gender)) + geom_boxplot() + stat_summary(fun=mean, geom="point") +facet_grid(. ~ Subject)

#Race/Ethnicity
ggplot(student_data, aes(`Race/Ethnicity`, fill=`Race/Ethnicity`)) + geom_bar() + geom_text(stat='count', aes(label=..count..), vjust=-0.5) + labs(y='Student Count') 

ggplot(student_data, aes(x = student_data$`Test Preparation Course`)) +
  geom_bar(position = position_dodge(width = 0.8), binwidth = 25, fill="orange") +
  facet_wrap(~student_data$`Race/Ethnicity`) + 
  geom_text(stat='count', aes(label=..count..), vjust=2)  +
  ylab('Student Count') +
  xlab('Test Preparation Course') +
  labs(title ='Test Preparation Course by Race/Ethnicity') +
  theme(legend.position = "none")

ggplot(student_data_w_scores, aes(x=`Race/Ethnicity`, y=`Score`, fill=`Race/Ethnicity`)) + geom_boxplot() + stat_summary(fun=mean, geom="point") + facet_grid(. ~ Subject)   

#Scores
require(gridExtra)
math <- ggplot(student_data,aes(`Math Score`)) + geom_histogram(fill='lightblue', color='blue') + ylab("Student Count")
reading <- ggplot(student_data,aes(`Reading Score`)) + geom_histogram(fill='lightblue', color='blue')+ ylab("Student Count")
writing <-ggplot(student_data,aes(`Writing Score`)) + geom_histogram(fill='lightblue', color='blue') + ylab("Student Count")
grid.arrange(math,reading,writing, nrow=3)

#Lunch
ggplot(student_data, aes(x=Lunch, y=`Average Score`, fill=Lunch)) + geom_boxplot() + stat_summary(fun=mean, geom="point")
lunch_avg <- lm(`Average Score` ~ Lunch, student_data)
summary(lunch_avg)
#H0 = there is no change in averages from lunches
#H1 = there is a change in averages from lunches
#As the p-value is much less than 0.05, we reject the null hypothesis 
#Therefore, there is a significant relationship between the variables of average score and lunch.


require(gridExtra)
p1 <- ggplot(student_data, aes(x=Lunch, y=`Math Score`, fill=Lunch)) + 
  geom_boxplot() +
  theme(legend.position = "none") +
  stat_summary(fun=mean, geom="point") 

p2 <-  ggplot(student_data, aes(x=Lunch, y=`Reading Score`, fill=Lunch)) + 
  geom_boxplot() +
  theme(legend.position = "none") +
  stat_summary(fun=mean, geom="point") 

p3 <-  ggplot(student_data, aes(x=Lunch, y=`Writing Score`, fill=Lunch)) + 
  geom_boxplot() +
  theme(legend.position = "none") +
  stat_summary(fun=mean, geom="point") 
grid.arrange(p1,p2,p3, ncol = 3)

ggplot(student_data_w_scores, aes(x=Lunch, y=Score, fill=Lunch)) + geom_boxplot() + stat_summary(fun=mean, geom="point") + facet_grid(. ~ Subject) 

#Test Preparation Course
ggplot(student_data, aes(x=`Test Preparation Course`, y=`Average Score`, fill=`Test Preparation Course`)) + 
  geom_boxplot() +
  stat_summary(fun=mean, geom="point") 

#Correlation
ggplot(student_data) + geom_point(aes(x=`Math Score`, y=`Reading Score`))
ggplot(student_data) + geom_point(aes(x=`Writing Score`, y=`Reading Score`))
ggplot(student_data) + geom_point(aes(x=`Reading Score`, y=`Writing Score`))
ggplot(student_data) + geom_point(aes(x=`Math Score`, y=`Writing Score`))

?lm
lunch_avg <- lm(`Average Score` ~ Lunch, student_data)
summary(lunch_avg)

read_write <- lm(`Reading Score` ~ `Writing Score`, student_data)
math_write <- lm(`Math Score` ~ `Writing Score`, student_data)
read_math<- lm(`Reading Score` ~ `Math Score`, student_data)
summary(read_write)
summary(math_write)
summary(read_math)

install.packages("ggpubr")
library("ggpubr")
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
educ_order <- c("master's degree","bachelor's degree", "associate's degree","some college", "high school", "some high school")

ggplot(data = student_data, aes(fct_relevel(`Parental Level of Education`,educ_order), fill=fct_relevel(`Parental Level of Education`, educ_order))) + geom_bar() + geom_text(stat='count', aes(label=..count..), vjust=-0.5) + labs(y="Student Count", x="Parental Level of Education") + guides(fill=guide_legend(title="Parental Level of Education"))
ggplot(student_data, aes(Gender, fill=Gender)) + geom_bar() +  geom_text(stat='count', aes(label=..count..), vjust=2)  + facet_wrap(vars(`Parental Level of Education`)) + labs(y='Student Count') + theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank()) 

ggplot(student_data_w_scores, aes(y = Score, x = fct_relevel(`Parental Level of Education`,educ_order), fill=fct_relevel(`Parental Level of Education`, educ_order))) +
  geom_boxplot() + 
  stat_summary(fun=mean, geom="point") +
  facet_grid(. ~ Subject) + labs(x="Parental Level of Education") +
  scale_x_discrete(guide = guide_axis(angle = 80)) + 
  guides(fill=guide_legend(title="Parental Level of Education"))

avg_grades_by_parentaleduc <- student_data %>% group_by(fct_relevel(`Parental Level of Education`, educ_order)) %>% summarize(`Avg Math Score` = mean(`Math Score`), `Avg Reading Score` = mean(`Reading Score`), `Avg Writing Score` = mean(`Writing Score`)) 
colnames(avg_grades_by_parentaleduc)[1] = "Parental Level of Education"
avg_grades_by_parentaleduc
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

#https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

mens_scores %>% group_by(Subject) %>% summarize(min = min(Score), max=max(Score), sd = sd(Score), mode = Mode(Score), mean = mean(Score))
females_scores %>% group_by(Subject) %>% summarize(min = min(Score), max=max(Score), sd = sd(Score), mode = Mode(Score), mean=mean(Score))

mens_scores %>% group_by(Subject) %>% summarize( mean = mean(Score))
females_scores %>% group_by(Subject) %>% summarize(mean=mean(Score))


females_scores %>% group_by(Subject) %>% filter(Score == 0) %>% count()
mens_scores  %>% group_by(Subject) %>% filter(Score == 0) %>% count()
females_scores %>% group_by(Subject) %>% filter(Score < 50) %>% count()
mens_scores  %>% group_by(Subject) %>% filter(Score < 50) %>% count()

gc()

ggplot(student_data_w_scores, aes(x=Gender, y=Score, fill=Gender)) + geom_boxplot() + stat_summary(fun=mean, geom="point") +facet_grid(. ~ Subject)

#Race/Ethnicity
ggplot(student_data, aes(`Race/Ethnicity`, fill=`Race/Ethnicity`)) + geom_bar() + geom_text(stat='count', aes(label=..count..), vjust=-0.5) + labs(y='Student Count') 

ggplot(student_data, aes(x = student_data$`Test Preparation Course`)) +
  geom_bar(position = position_dodge(width = 0.8), binwidth = 25, fill="orange") +
  facet_wrap(~student_data$`Race/Ethnicity`, ncol = 5) + 
  geom_text(stat='count', aes(label=..count..), vjust=2)  +
  ylab('Student Count') +
  xlab('Test Preparation Course') +
  labs(title ='Test Preparation Course by Race/Ethnicity') 

test_prep_by_race <- student_data %>% group_by(`Race/Ethnicity`, `Test Preparation Course`) %>% count(`Test Preparation Course`) %>% spread(key = `Test Preparation Course`, value=n) 
test_prep_by_race <- test_prep_by_race %>% mutate(`% Completed`  = completed/(none+completed))
test_prep_by_race

ggplot(student_data_w_scores, aes(x=`Race/Ethnicity`, y=`Score`, fill=`Race/Ethnicity`)) + geom_boxplot() + stat_summary(fun=mean, geom="point") + facet_grid(. ~ Subject)   
student_data_w_scores %>% group_by(`Race/Ethnicity`, Subject) %>% select(Subject, Score) %>% summarize(`Avg Score`= mean(Score)) %>% arrange(Subject, desc(`Avg Score`))

#Scores
require(gridExtra)
math <- ggplot(student_data,aes(`Math Score`)) + geom_histogram(fill='lightblue', color='blue') + ylab("Student Count")
reading <- ggplot(student_data,aes(`Reading Score`)) + geom_histogram(fill='lightblue', color='blue')+ ylab("Student Count")
writing <-ggplot(student_data,aes(`Writing Score`)) + geom_histogram(fill='lightblue', color='blue') + ylab("Student Count")
grid.arrange(math,reading,writing, nrow=3)

#Lunch
ggplot(student_data,aes(Lunch, fill=Lunch)) + geom_bar() + geom_text(stat='count', aes(label=..count..), vjust=2) +ylab("Student Count")
ggplot(student_data, aes(x=Lunch, y=`Average Score`, fill=Lunch)) + geom_boxplot() + stat_summary(fun=mean, geom="point")

avg_lunch <- student_data %>% group_by(Lunch) %>% summarize(`Average Score` = mean(`Average Score`))
avg_lunch

avg_lunch_persubj <- student_data %>% group_by(Lunch) %>% summarize(`Avg Math Score` = mean(`Math Score`), `Avg Reading Score`= mean(`Reading Score`), `Avg Writing Score`=mean(`Writing Score`))
avg_lunch_persubj

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

student_data %>% group_by(`Test Preparation Course`) %>% summarize("Avg Math Score" = mean(`Math Score`), "Avg Writing Score" = mean(`Writing Score`), 'Avg Reading Score' = mean(`Reading Score`))

#Correlation
ggplot(student_data) + geom_point(aes(x=`Math Score`, y=`Reading Score`))
ggplot(student_data) + geom_point(aes(x=`Writing Score`, y=`Reading Score`))
ggplot(student_data) + geom_point(aes(x=`Reading Score`, y=`Writing Score`))
ggplot(student_data) + geom_point(aes(x=`Math Score`, y=`Writing Score`))

#http://www.sthda.com/english/wiki/correlation-test-between-two-variables-in-r
ggscatter(student_data, x = "Math Score", y = "Reading Score", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")
ggscatter(student_data, x = "Reading Score", y = "Writing Score", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")
ggscatter(student_data, x = "Math Score", y = "Writing Score", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")

cor(student_data$`Math Score`, student_data$`Reading Score`, method="pearson")
cor(student_data$`Reading Score`, student_data$`Writing Score`, method="pearson")
cor(student_data$`Math Score`, student_data$`Writing Score`, method="pearson")

shapiro.test(student_data$`Math Score`)
shapiro.test(student_data$`Writing Score`)
shapiro.test(student_data$`Reading Score`)

read_write <- lm(`Reading Score` ~ `Writing Score`, student_data)
math_write <- lm(`Math Score` ~ `Writing Score`, student_data)
read_math<- lm(`Reading Score` ~ `Math Score`, student_data)
summary(read_write)
summary(math_write)
summary(read_math)
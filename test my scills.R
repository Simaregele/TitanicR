setwd("C:/Users/mrsim/Google ����/R statistic/titanic")

library(ggplot2)

df_train <- read.csv("train.csv")
df_test <- read.csv('test.csv')

df_train$Train = T
df_test$Train = F

str(df_train)

hist(df_test$Age)
hist(df_train$Age)

#�������� �� ������������ Age
shap_test <- shapiro.test(df$Age)


df_test$Survived <- NA
df <- rbind(df_train, df_test)

# ������� 3 �������� � ����������� �� ���� ����� ��� ��������
table(df$Embarked)

# ���� 2 NA ������ ������� ������������� ���������
df_subset <- df
df_subset$PassengerId <- NULL
df_subset$Name <- NULL
df_subset$Ticket <- NULL
df_subset$Train <- NULL
df_subset$Embarked <- NULL
df_subset$Sex <- NULL

# ����� �������� ��� ����� ������� ��� �� ���
df_subset$Embarked_S <- ifelse(df_subset$Embarked == "S", 1, 0)
df_subset$Embarked_Q <- ifelse(df_subset$Embarked == "Q", 1, 0)
df_subset$Embarked_C <- ifelse(df_subset$Embarked == "C", 1, 0)

# ������ ���� ����� �� ��� ���� ���� �� �����
df_subset$Sex <- ifelse(df_subset$Sex == "male", 1, 0)
log_reg <- glm(Embarked_S +  Embarked_Q + Embarked_C ~ Age, df_subset, family = "binomial")
df$Embarked <- ifelse(is.na(df$Embarked), prediction(log_reg, df))
summary(log_reg)

# 
df_subset$Embarked_S <- ifelse(df_subset$Embarked == "S", 1, 0)
df_subset$Embarked_Q <- ifelse(df_subset$Embarked == "Q", 1, 0)
df_subset$Embarked_C <- ifelse(df_subset$Embarked == "C", 1, 0)


#�������� ��������� ��� AGE
all_clean <- subset(all_data, is.na(Age) == F)
is.na(all_clean$Age)
model <-  lm(Age ~ Fare + Have_sib + Survived + Have_parch + Pclass, df_train)
0.2566
age_model <- lm(Age ~ SibSp + Pclass, all_data)
summary(age_model)

# ����������� Age
all_data$Age <- ifelse(is.na(all_data$Age), 
                       predict(age_model, all_data), print(all_data$Age))

# ��������� �� ������������� �������� � ������ ������������� 
all_data$Age <- ifelse(all_data$Age <= 0, 
                       all_data$Age * -1, print(all_data$Age))

all_data$Embarked[all_data$Embarked==""] <- "S"


table(is.na(all_data$Fare))
# ��� ��� �������� ����������
all_data$Fare[is.na(all_data$Fare)] <- 10


# ��������� ��� �� ������

df_train <- all_data[is.na(all_data$Survived)==F,]
df_test <- all_data[is.na(all_data$Survived)==T,]

# ��������

library(neuralnet)
neural <- neuralnet(Survived ~ SibSp + Age + Parch + Pclass, hidden=4, df_train)
plot(neural)

library(randomForest)
forest <- randomForest(Survived ~ SibSp + Age + Parch + Pclass, ntree)

df_anser <- data.frame(PassengerId = df_test$PassengerId, 
                            Survived = ifelse(predict(neural, df_test) > 0.5,
                              1, 0))


write.csv(df_anser, "anser.csv", row.names = F)

# �������� ���� �������� ������



# �������� �� ��������� ������
# ������� �������� �� �������

hist(all_data$Age[all_data$Have_sib == T])+
  hist(all_data$Age[all_data$Have_sib == F])

shapiro.test(all_data$Age)

t.test(Age ~ Have_sib, all_data)

fit <- aov(Age ~ Fare*Embarked, all_data)
summary(fit)

fit2 <- aov(Age ~ Have_sib, all_data)
summary(fit2)

ggplot(all_data, aes(x = Have_sib, y = Age))+
  geom_boxplot()


ggplot(all_data, aes(x = Have_sib, y = Fare)) + 
  geom_boxplot()

?ggplot

str(all_data)

ggplot(all_data, aes(x = Age, fill = Have_sib))+
  geom_dotplot(alpha(0.5))

ggplot(all_data, aes(x = Age, fill = Survived))+
  geom_dotplot(alpha(0.5))


ggplot(df_train, aes(x = Survived, y = Age, fill = Sex))+
  geom_boxplot()

lm()


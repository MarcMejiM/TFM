# *************************************************************************
# Machine Learning para la predicción del Alzheimer - 
# TFM: Machine Learning en biomarcadores y análisis de datos clínicos
# Authors: Marc Mejías Muñoz
# Data: 
# Description:
#     Prepocesamiento de los datos
# *************************************************************************

# PACKAGES----------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(car)
library(corrplot)

#PREPROCESS---------------------------------------------------------------------
## Load data
setwd('C:/Users/Marc/OneDrive/Escritorio/Marc/TFM')
data <- read.csv('alzheimers_disease_data.csv')

# First look
str(data)

# Duplicated values
nrow(data[duplicated(data), ])

# Conversion as numeric
data$Age<-as.numeric(as.integer(data$Age))
data$SystolicBP<-as.numeric(as.integer(data$SystolicBP))
data$DiastolicBP<-as.numeric(as.integer(data$DiastolicBP))

# Conversion as factor
data <- data %>% 
  mutate_if(is.integer,as.factor)
str(data)
summary(data)

# Missing values
na_table <- data.frame(Variable = names(data),NA_Count = colSums(is.na(data)))
write.csv(na_table, "na_table.csv", row.names = FALSE)

# Outliers
par(mfrow = c(2, 3)) 

boxplot(data$SystolicBP, main = "Systolic BP")
boxplot(data$DiastolicBP, main = "Diastolic BP")
boxplot(data$CholesterolHDL, main = "Cholesterol HDL")
boxplot(data$CholesterolLDL, main = "Cholesterol LDL")
boxplot(data$CholesterolTotal, main = "Total Cholesterol")
boxplot(data$CholesterolTriglycerides, main = "Cholesterol Triglycerides")

# Labels
data$Gender <- factor(data$Gender, levels = c(0, 1), 
                      labels = c("Male", "Female"))
data$Ethnicity <- factor(data$Ethnicity, levels = c(0, 1, 2, 3), 
                         labels = c("Caucasica", "Afroamericana",
                                    "Asiatica", "Otra"))
data$EducationLevel <- factor(data$EducationLevel, levels = c(0, 1, 2, 3), 
                         labels = c("Ninguna", "Secundaria",
                                    "Universitaria", "Superior"))
data$Smoking <- factor(data$Smoking, levels = c(0, 1), 
                      labels = c("No", "Si"))
data$FamilyHistoryAlzheimers <- factor(data$FamilyHistoryAlzheimers, 
                                       levels = c(0, 1), labels = c("No", "Si"))
data$CardiovascularDisease <- factor(data$CardiovascularDisease, 
                                     levels = c(0, 1), labels = c("No", "Si"))
data$Diabetes <- factor(data$Diabetes, levels = c(0, 1), 
                       labels = c("No", "Si"))
data$Depression <- factor(data$Depression, levels = c(0, 1), 
                       labels = c("No", "Si"))
data$HeadInjury <- factor(data$HeadInjury, levels = c(0, 1), 
                       labels = c("No", "Si"))
data$Hypertension <- factor(data$Hypertension, levels = c(0, 1), 
                       labels = c("No", "Si"))
data$MemoryComplaints <- factor(data$MemoryComplaints, levels = c(0, 1), 
                       labels = c("No", "Si"))
data$BehavioralProblems <- factor(data$BehavioralProblems, levels = c(0, 1), 
                                labels = c("No", "Si"))
data$Confusion <- factor(data$Confusion, levels = c(0, 1), 
                                labels = c("No", "Si"))
data$Disorientation <- factor(data$Disorientation, levels = c(0, 1), 
                                labels = c("No", "Si"))
data$PersonalityChanges <- factor(data$PersonalityChanges, levels = c(0, 1), 
                                labels = c("No", "Si"))
data$DifficultyCompletingTasks <- factor(data$DifficultyCompletingTasks, 
                                         levels = c(0, 1), 
                                         labels = c("No", "Si"))
data$Forgetfulness <- factor(data$Forgetfulness, levels = c(0, 1), 
                                labels = c("No", "Si"))
data$Diagnosis <- factor(data$Diagnosis, levels = c(0, 1), 
                                labels = c("No", "Si"))
# Drop variables
data2 <- select(data, -PatientID, -DoctorInCharge)
str(data2)
write.csv(data2, "data_full.csv", row.names = FALSE)

# DATA EXPLORATION--------------------------------------------------------------
# Clasification variable
diag_table <- table(data2[["Diagnosis"]])
write.csv(diag_table, "diag_table.csv", row.names = FALSE)

diag_df <- data.frame(prop.table(diag_table))
names(diag_df) <- c("Diagnosis", "Prop")

diag_df$Porcentaje <- round(diag_df$Prop*100,2)

ggplot(diag_df, aes(x="", y=Prop, fill =Diagnosis)) +
  geom_col(width = 1, color= "white") +
  geom_text(aes(label = Porcentaje), color = c("darkblue", "darkblue"),
            position = position_stack(vjust = 0.5),
            show.legend = FALSE) +
  guides(fill= guide_legend(title = "Diagnosis")) +
  scale_fill_brewer() +
  coord_polar(theta = "y") +
  theme_void(base_family =  "Arial" )

# Categorical variables
data_cat <- data2 %>% select(where(is.factor), -Diagnosis)
str(data_cat)

## Freq table
dir.create("Freq_table")
for (var in names(data_cat)) {
  freq_table <- table(data_cat[[var]])
  
  freq_df <- data.frame(cat = names(freq_table), 
                        Frecuencia=as.vector(freq_table))
  
  names(freq_df)[1] <- var
  
  write.csv(freq_df,
            file = paste0("Freq_table/", var, "_frequencies.csv"),
            row.names = FALSE, quote = FALSE)
}

## Freq Chart
dir.create("Freq_plots")

for (var in names(data_cat)) {
  p <- ggplot(data_cat, aes(x = .data[[var]])) +
    geom_bar(fill = "lightblue", color = "darkblue") +
    labs(
      title = paste("Distribución de", var),
      x = var,
      y = "Frecuencia"
    ) +
    theme_minimal(base_family = "Arial") +
    theme(
      plot.title = element_text(face = "bold", size = 16, color = "#000078"),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 14)
    )
  
  ggsave(
    filename = paste0("Freq_plots/", var, "_barplot.png"),
    plot = p,
    width = 6, height = 4
  )
}

# Quantitative variables
## Histogram chart
data_num <- data2 %>% select(where(is.numeric))

dir.create("hist_plots")

for (var in names(data_num)) {
  p <- ggplot(data_num, aes(x = .data[[var]])) +
    geom_histogram(fill = "lightblue", color = "darkblue") +
    labs(
      title = paste("Distribución de", var),
      x = var,
      y = "Frecuencia"
    ) +
    theme_minimal(base_family = "Arial") +
    theme(
      plot.title = element_text(face = "bold", size = 16, color = "#000078"),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 14)
    )
  
  ggsave(
    filename = paste0("hist_plots/", var, "_histplot.png"),
    plot = p,
    width = 6, height = 4
  )
}

# Correlation
cor_res <- cor(data_num)

png("corr_plot.png", width = 1600, height = 1600)
corrplot(cor_res,method="color",tl.col="darkblue", tl.srt=30, order = "AOE", 
         number.cex=1.2,tl.cex=1.5, addCoef.col = "black")

dev.off()

# Contrast hypothesis
var_int <- data2 %>% select(BehavioralProblems, MemoryComplaints, 
                               DifficultyCompletingTasks, PersonalityChanges,
                               Disorientation, FamilyHistoryAlzheimers, 
                               CardiovascularDisease, EducationLevel, HeadInjury)



## Contingency tables

dir.create("freq_plots_2")

for (var in names(var_int)) {
  p <- ggplot(data2, aes(x = .data[[var]], fill = Diagnosis)) + 
    geom_bar(position = "fill", color= "white") +
    scale_fill_manual(values = c("No"="lightblue", "Si"="darkblue")) +
    labs(
      title = paste("Diagnosis según", var),
      x = var,
      y = "Proporción"
    ) +
    theme_minimal(base_family = "Arial") +
      theme(
        plot.title = element_text(face = "bold", size = 16, color = "#000078"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14)
      )
        
  ggsave(
    filename = paste0("freq_plots_2/", var, "_freqplot.png"),
    plot = p,
    width = 6, height = 4
  )
}

## Chi test
for (var in names(data_cat)) {
  var_int_diag <- table(data2[[var]], data2$Diagnosis)
  chi_test <- chisq.test(var_int_diag)
  
  cat("\n---", var, "----\n")
  print(var_int_diag)
  print(chi_test)
}

var_int_num <- data2 %>% select(Age, MMSE)
dir.create("hist_plots_2")

## Histogram chart 2
for (var in names(var_int_num)) {
  p <- ggplot(data2, aes(x=.data[[var]], fill=Diagnosis)) + 
    geom_histogram(color = "white", alpha=0.5, position="identity") + 
    labs(
      title = paste("Distribución Diagnosis según", var),
      x = var,
      y = "Frecuencia"
    ) +
    scale_fill_manual(values = c("No"="lightblue", "Si"="darkblue")) +
    theme_minimal(base_family = "Arial") +
    theme(
      plot.title = element_text(face = "bold", size = 14, color = "#000078"),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 14)
    )
  
  ggsave(
    filename = paste0("hist_plots_2/", var, "_histplot.png"),
    plot = p,
    width = 6, height = 4
  )  
}

## Shapiro test
for (var in names(data_num)) {
  x <- data2[[var]]
  sw_test <- shapiro.test(x)
  
  cat("\n---", var, "----\n")
  print(sw_test)
}

## Levene-Fligner test
for (var in names(data_num)) {
  l_test <- leveneTest(data2[[var]] ~ data2$Diagnosis)
  fk_test <- fligner.test(data2[[var]] ~ data2$Diagnosis)
  
  cat("\n---", var, "----\n")
  print(l_test)
  print(fk_test)
}

## T student test
data_par <- data_num %>% select(-MMSE, -FunctionalAssessment, -ADL)
for (var in names(data_par)) {
  t_test <- t.test(data2[[var]] ~ data2$Diagnosis)
  
  cat("\n---", var, "----\n")
  print(t_test)
}

## Wilcox test
data_npar <- data_num %>% select(MMSE, FunctionalAssessment, ADL)
for (var in names(data_npar)) {
  w_test <- wilcox.test(data2[[var]] ~ data2$Diagnosis)
  
  cat("\n---", var, "----\n")
  print(w_test)
}

## Histogram charts 3
var_sig <- data2 %>% select(SleepQuality, CholesterolHDL, FunctionalAssessment,
                            ADL)
dir.create("hist_plots_3")

for (var in names(var_sig)) {
  p <- ggplot(data2, aes(x=.data[[var]], fill=Diagnosis)) + 
    geom_histogram(color = "white", alpha=0.5, position="identity") + 
    labs(
      title = paste("Distribución Diagnosis según", var),
      x = var,
      y = "Frecuencia"
    ) +
    scale_fill_manual(values = c("No"="lightblue", "Si"="darkblue")) +
    theme_minimal(base_family = "Arial") +
    theme(
      plot.title = element_text(face = "bold", size = 16, color = "#000078"),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 14)
    )
  
  ggsave(
    filename = paste0("hist_plots_3/", var, "_histplot.png"),
    plot = p,
    width = 6, height = 4
  )  
}

## glm models
data2$Diag <- ifelse(data2$Diagnosis == "No", 0, 1)
prop.table(table(data2$Diag))

data3 <- data2 %>% select(-Diagnosis)
mod1 <- glm(Diag ~ ., data = data3, family = binomial())
summary(mod1)

data4 <- data3 %>% select(Diag, MMSE, MemoryComplaints, BehavioralProblems,
                          CholesterolLDL, CholesterolHDL, FunctionalAssessment,
                          SleepQuality, ADL)

mod2 <- glm(Diag ~ ., data = data4, family = binomial())
summary(mod2)

mod3 <- glm(Diag ~ . -SleepQuality, data = data4, family = binomial())
summary(mod3)

mod4 <- glm(Diag ~ . -CholesterolLDL, data = data4, family = binomial())
summary(mod4)

mod5 <- glm(Diag ~ . -CholesterolHDL, data = data4, family = binomial())
summary(mod5)

mod6 <- glm(Diag ~ . -CholesterolHDL -SleepQuality, data = data4, 
            family = binomial())
summary(mod6)

mod7 <- glm(Diag ~ . -CholesterolLDL -SleepQuality, data = data4, 
            family = binomial())
summary(mod7)

write.csv(data4, "data_sig.csv", row.names = FALSE)

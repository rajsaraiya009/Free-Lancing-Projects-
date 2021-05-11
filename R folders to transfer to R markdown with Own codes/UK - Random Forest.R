### Model 2: After Decision, use Random Forest for another categorical pred model
start_time <- Sys.time()

caret_random_forest <- train(x=train_df_categorical[, !names(train_df_categorical) %in% 'Accident_Severity'], y=train_df_categorical$Accident_Severity,
                             data=train_df_categorical, method='rf', tuneGrid=expand.grid(.mtry=c(5:10)),
                             trControl=trainControl(method="cv", number=10))
end_time <- Sys.time()
rf_runtime <- round(as.numeric(end_time - start_time), 2)
plot(caret_random_forest)

#test
test_df_categorical$pred <- predict(caret_random_forest, test_df_categorical)
rf_confusion_matrix <- confusionMatrix(data=test_df_categorical$pred, reference=test_df_categorical$Accident_Severity)
test_df_categorical <- test_df_categorical %>%
  mutate(pred = predict(caret_random_forest, test_df_categorical),
         pred_prob = predict(caret_random_forest, type="prob", test_df_categorical)[,2],
         error = ifelse(pred != Accident_Severity, 1, 0))

#ROC plot
roc <- test_df_categorical %>%
  select(Accident_Severity, pred_prob) %>%
  mutate(Accident_Severity = as.numeric(Accident_Severity) - 1,
         Accident_Severity.str = c("Fatal_Serious", "Slight")[Accident_Severity + 1]) %>%
  ggplot(aes(d = Accident_Severity, m = pred_prob)) +
  geom_roc(labels = FALSE)
roc +
  style_roc(theme = theme_bw, xlab = "False Positive Rate", ylab = "True Positive Rate") +
  theme(panel.grid.major = element_blank(), panel.border = element_blank(),
        axis.line = element_line(colour = "grey")) +
  ggtitle("Random Forest - ROC Curve") +
  annotate("text", x = .75, y = .25,
           label = paste("AUROC =", round(calc_auc(roc)$AUC, 3)))

#score list
rf_accuracy <- round(rf_confusion_matrix$overall['Accuracy'], 3)
rf_precision <- round(rf_confusion_matrix$byClass['Pos Pred Value'], 3)
rf_recall <- round(rf_confusion_matrix$byClass['Sensitivity'], 3)
rf_f1_score <- round(2*((rf_precision * rf_recall) / (rf_precision + rf_recall)), 3)
rf_roc <- round(calc_auc(roc)$AUC, 3)

#use of test
test_df_categorical <- test_df_categorical[ , !(names(test_df_categorical) %in% c('pred', 'pred_prob', 'error'))]

#accuracy readings
data.frame(accuracy_baseline, rf_accuracy, rf_precision, rf_recall, rf_f1_score, rf_roc)

## Variable Importance
rf_imp <- varImp(caret_random_forest, scale=FALSE)
rf_imp <- rf_imp$importance
rf_gini <- data.frame(Variables=row.names(rf_imp), MeanDecreaseGini=rf_imp$Overall)
rf_importance_plot <- ggplot(rf_gini, aes(x=reorder(Variables, MeanDecreaseGini), y=MeanDecreaseGini, fill=MeanDecreaseGini)) +
  geom_bar(stat='identity') + coord_flip() + theme(legend.position="none") + labs(x="") +
  ggtitle('Variable Importance in the Random Forest model') + theme(plot.title = element_text(hjust=0.5))
rf_importance_plot
rm(rf_imp, rf_gini)

#compare VI of RF and DT
grid.arrange(dt_importance_plot, rf_importance_plot, ncol=2)
rm(dt_importance_plot, rf_importance_plot)

#removing noise
set.seed(123)
sample <- createDataPartition(sampled_df_categorical$Accident_Severity, p=0.5, list=FALSE)
sampled_df_numerical <- sampled_df_categorical[sample, ]

#removing the noise according to the variable importance of the Decision Tree and Random Forest models
sampled_df_numerical <- as_tibble(sampled_df_numerical[ , !(names(sampled_df_numerical) %in%
                                                              c('High_Wind', 'Propulsion_Code'))])
sampled_df_numerical <- dummy_cols(sampled_df_numerical,
                                   select_columns = c('Region', 'X1st_Road_Class', 'Urban_or_Rural_Area',
                                                      'Road_Surface_Conditions', 'Road_Type', 'Weather',
                                                      'Lights', 'Junction_Detail',
                                                      'Junction_Location', 'X1st_Point_of_Impact',
                                                      'Driver_Journey_Purpose', 'Vehicle_Make',
                                                      'Vehicle_Category', 'Vehicle_Manoeuvre'),
                                   remove_most_frequent_dummy=TRUE)
columns_to_drop <- c('Region', 'X1st_Road_Class', 'Urban_or_Rural_Area', 'Road_Surface_Conditions', 'Road_Type',
                     'Weather', 'Lights', 'Junction_Detail', 'Junction_Location', 'X1st_Point_of_Impact',
                     'Driver_Journey_Purpose', 'Vehicle_Make', 'Vehicle_Category', 'Vehicle_Manoeuvre')

sampled_df_numerical <- sampled_df_numerical[ , !(names(sampled_df_numerical) %in% columns_to_drop)]
rm(columns_to_drop)

sampled_df_normalized <- predict(preProcess(sampled_df_numerical, method=c("center", "scale")),
                                 sampled_df_numerical)

sampled_df_normalized <- dummy_cols(sampled_df_normalized, select_columns = c('Accident_Severity'),
                                    remove_most_frequent_dummy=TRUE)
names(sampled_df_normalized)[names(sampled_df_normalized) == 'Accident_Severity_Fatal_Serious'] <- 'Fatal_or_Serious_Accident'

sampled_df_numerical <- select(sampled_df_normalized, -matches("Accident_Severity"))

sampled_df_normalized <- select(sampled_df_normalized, -matches("Fatal_or_Serious_Accident"))

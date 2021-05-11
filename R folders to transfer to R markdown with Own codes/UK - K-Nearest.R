#K nearest neighbour (using normalized data)
set.seed(17245)
sample <- createDataPartition(sampled_df_normalized$Accident_Severity, p=0.7, list=FALSE) 
train_df_normalized <- sampled_df_normalized[sample, ]
test_df_normalized <- sampled_df_normalized[-sample, ]

#set accuracy baseline
accuracy_baseline <- round(length(test_df_normalized[test_df_normalized$Accident_Severity == 'Slight', ]$Accident_Severity)/
                             length(test_df_normalized$Accident_Severity), 3)
accuracy_baseline

#train
start_time <- Sys.time()
knnFit <- train(Accident_Severity~., data=train_df_normalized, method="knn", tuneGrid=expand.grid(k=25:50),
                metric="Accuracy", trControl=trainControl(method="cv", number=10))
end_time <- Sys.time()
knn_runtime <- round(as.numeric(end_time - start_time), 2)
plot(knnFit)

test_df_normalized$Accident_Severity = as.factor(test_df_normalized$Accident_Severity)

##Test
test_df_normalized$pred <- predict(knnFit, test_df_normalized)
knn_confusion_matrix <- confusionMatrix(data=test_df_normalized$pred, reference=test_df_normalized$Accident_Severity)
test_df_normalized <- test_df_normalized %>%
  mutate(pred = predict(knnFit, test_df_normalized),
         pred_prob = predict(knnFit, type="prob", test_df_normalized)[,2],
         error = ifelse(pred != Accident_Severity, 1, 0))

#Roc
roc <- test_df_normalized %>%
  select(Accident_Severity, pred_prob) %>%
  mutate(Accident_Severity = as.numeric(Accident_Severity) - 1,
         Accident_Severity.str = c("Fatal_Serious", "Slight")[Accident_Severity + 1]) %>%
  ggplot(aes(d = Accident_Severity, m = pred_prob)) +
  geom_roc(labels = FALSE)
roc +
  style_roc(theme = theme_bw, xlab = "False Positive Rate", ylab = "True Positive Rate") +
  theme(panel.grid.major = element_blank(), panel.border = element_blank(),
        axis.line = element_line(colour = "grey")) +
  ggtitle("kNN - ROC Curve") +
  annotate("text", x = .75, y = .25,
           label = paste("AUROC =", round(calc_auc(roc)$AUC, 3)))

knn_accuracy <- round(knn_confusion_matrix$overall['Accuracy'], 3)
knn_precision <- round(knn_confusion_matrix$byClass['Pos Pred Value'], 3)
knn_recall <- round(knn_confusion_matrix$byClass['Sensitivity'], 3)
knn_f1_score <- round(2*((knn_precision * knn_recall) / (knn_precision + knn_recall)), 3)
knn_roc <- round(calc_auc(roc)$AUC, 3)
test_df_normalized <- test_df_normalized[ , !(names(test_df_normalized) %in% c('pred', 'pred_prob', 'error'))]
data.frame(accuracy_baseline, knn_accuracy, knn_precision, knn_recall, knn_f1_score, knn_roc)

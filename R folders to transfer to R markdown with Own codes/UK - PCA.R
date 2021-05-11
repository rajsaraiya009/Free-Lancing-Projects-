#Model PCA
#Looking into the dimension
pca_model <- PCA(sampled_df_numerical,
                 scale.unit=TRUE, ncp=2, graph=F)

eigenvalues <- get_eigenvalue(pca_model)
eigenvalues # Amount of variance retained by each principal component

scree_plot <- fviz_screeplot(pca_model, addlabels=T, ylim=c(0, 10)) # Graph of stdev explanation
scree_plot

pca_results <- get_pca_var(pca_model)

#plot biplot
set.seed(1715)
pca_km <- kmeans(pca_results$coord, centers=5, nstart=25)
groups <- as.factor(pca_km$cluster)
fviz_pca_var(pca_model, col.var=groups, legend.title="Cluster")
rm(accidents_df, sampled_df_numerical, eigenvalues, scree_plot, pca_results, pca_model, pca_km, groups)


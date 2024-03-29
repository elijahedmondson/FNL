# Bioconductor
library(FlowRepositoryR)
library(flowCore)
library(openCyto)
library(ggcyto)
library(FlowSOM)
library(CytoML)
# Tidyverse
library(fs)
library(janitor)
library(tidyverse)
library(reshape2)
# General purpose
library(curl)
# Visualization
library(umap)
### Loop preprocessing pipeline for all the files
### Loop preprocessing pipeline for all the files
### Loop preprocessing pipeline for all the files
### Loop preprocessing pipeline for all the files
### Loop preprocessing pipeline for all the files
### Loop preprocessing pipeline for all the files
### Loop preprocessing pipeline for all the files
### Loop preprocessing pipeline for all the files
dir_prepr <- "C:/Users/edmondsonef/Desktop/FACS/1-Preprocessed/" #where the preprocessed data will be stored
dir_QC <- "C:/Users/edmondsonef/Desktop/FACS/2-QC/" #where the data QC results will be stored
dir_RDS <- "C:/Users/edmondsonef/Desktop/FACS/3-RDS/" #where the R objects will be stored
dir_results <- "C:/Users/edmondsonef/Desktop/FACS/4-Results/" #where the results will be stored
dir_raw <- "C:/Users/edmondsonef/Desktop/FACS/" #where the raw data is located
path_comp <- "C:/Users/edmondsonef/Desktop/0-Autospill/table_compensation/autospill_compensation.csv" #where comp matrix is located
files <- list.files(path = dir_raw, pattern = "Samples")

wsp_file <- 'C:/Users/edmondsonef/Desktop/Humanized/Flow/5-02Mar2022/15719 02Mar2022 Simone.wsp'
ws <- open_flowjo_xml(wsp_file)
ws
fj_ws_get_samples(ws, group_id = 4)
gs <- flowjo_to_gatingset(ws, name = 4, path=dir_raw)
gh_pop_get_stats(gs[[1]], "/scatter/sing")
cs <- gs_pop_get_data(gs, "/scatter/sing")
fs <- cytoset_to_flowSet(cs)
dir <- "C:/Users/edmondsonef/Desktop/FACS/FLOWSET/"

fs@phenoData@data$name
write.flowSet(fs, outdir=dir, filename = fs@phenoData@data$name)
### Loop preprocessing pipeline for all the files
### Loop preprocessing pipeline for all the files
### Loop preprocessing pipeline for all the files
### Loop preprocessing pipeline for all the files
### Loop preprocessing pipeline for all the files
### Loop preprocessing pipeline for all the files
### Loop preprocessing pipeline for all the files
### Loop preprocessing pipeline for all the files







# 1. Load the libraries# 1. Load the librariesdesc()
library(flowCore)
library(FlowSOM)
library(ggplot2)

# 2. Define the general and preprocessing variables
data_dir <- "C:/Users/edmondsonef/Desktop/FACS/FLOWSET/"
#wsp_file <- paste0(data_dir, '15708 15Feb2022 Simone.wsp')
file_pattern <- "\\d.fcs" #digit at the end and fcs extension
reference_file <- read.FCS(paste0(data_dir, 'Samples_Tube_015 Animal 142_027.fcs'), truncate_max_range = FALSE)
reference_marker <- "PE-A" # Scatter values will be scaled to have the same range

markers_of_interest <- c("BB515-A",
                         "BB700-P-A",
                         "APC-A",
                         "APC-Cy7-A",
                         "BV421-A",
                         "BV786-A",
                         "BUV395-A",
                         "BUV805-A",
                         "PE-A",
                         "PE-CF594-A",
                         "PE-Cy7-A")

# 3. Define and create the directories
dir_prepr <- "C:/Users/edmondsonef/Desktop/FACS/FLOWSET/1-Preprocessed/" #where the preprocessed data will be stored
dir_QC <- "C:/Users/edmondsonef/Desktop/FACS/FLOWSET/2-QC/" #where the data QC results will be stored
dir_RDS <- "C:/Users/edmondsonef/Desktop/FACS/FLOWSET/3-RDS/" #where the R objects will be stored
dir_results <- "C:/Users/edmondsonef/Desktop/FACS/FLOWSET/4-Results/" #where the results will be stored
dir_raw <- "C:/Users/edmondsonef/Desktop/FACS/FLOWSET/" #where the raw data is located
path_comp <- "C:/Users/edmondsonef/Desktop/0-Autospill/table_compensation/autospill_compensation.csv" #where comp matrix is located

for (path in c(dir_prepr, dir_QC, dir_RDS, dir_results)){
  dir.create(path)
}

# 4. Prepare some additional information for preprocessing the files 
# given the variable choices of step 2.
files <- list.files(path = dir_raw, pattern = "Samples")
files
channels_of_interest <- GetChannels(object = reference_file,
                                    markers = markers_of_interest, 
                                    exact = FALSE)
compensation_matrix <- read.csv(path_comp, 
                                check.names = FALSE, row.names = 1)

colnames(compensation_matrix) <- sub(" :: .*", "",         
                                     colnames(compensation_matrix))

# Compute transformation list
ff_m <- PeacoQC::RemoveMargins(reference_file, channels_of_interest)
names(ff_m)
#exprs(ff_m)
each_col(ff_m, median)

ff_c <- flowCore::compensate(ff_m, compensation_matrix)
translist <- estimateLogicle(ff_c, colnames(compensation_matrix))
ff_t <- flowCore::transform(ff_c, translist)
q5_goal <- quantile(exprs(ff_t)[,reference_marker], 0.05)
q95_goal <- quantile(exprs(ff_t)[,reference_marker], 0.95)
q5_SSCA <- quantile(exprs(ff_t)[,"SSC-A"], 0.05)
q95_SSCA <- quantile(exprs(ff_t)[,"SSC-A"], 0.95)
SSCA_a <- (q95_goal - q5_goal) / (q95_SSCA - q5_SSCA)
SSCA_b <- q5_goal - q5_SSCA * (q95_goal - q5_goal) / (q95_SSCA - q5_SSCA)
translist <- c(translist, 
               transformList("SSC-A", flowCore::linearTransform(a = SSCA_a,
                                                                b = SSCA_b)))
translist
#rm(ff_c, ff_m, ff_t, reference_file)

# 5. Read the first fcs file into a flowframe
ff <- read.FCS(paste0(dir_raw, files[2]), truncate_max_range = FALSE)

# 6. Remove margin events
ff_m <- PeacoQC::RemoveMargins(ff, channels_of_interest)

# 7. Compensate
ff_c <- flowCore::compensate(ff_m, compensation_matrix)

# 8. Transform, logicle for marker channels, linear for scatter channel
ff_t <- flowCore::transform(ff_c, translist)

# 9. Remove doublets and filter live cells
ff_s <- PeacoQC::RemoveDoublets(ff_t)
#selected_live <- flowCore::filter(ff_s, live_gate)
#ff_l <- ff_s[selected_live@subSet, ]

# 10. QC with PeacoQC
PQC <- PeacoQC::PeacoQC(ff = ff_s,
                        channels = channels_of_interest,
                        plot = TRUE, save_fcs = FALSE,
                        output_directory = dir_QC)

# 11. Save the preprocessed data
write.FCS(PQC$FinalFF,
          file = paste0(dir_prepr, files[1]))

# 12. Visualize the preprocessing
filter_plot <- function(ff_pre, ff_post, title, channel_x, channel_y){
  df <- data.frame(x = exprs(ff_pre)[,channel_x],
                   y = exprs(ff_pre)[,channel_y])
  i <- sample(nrow(df), 10000)
  if (!"Original_ID" %in% colnames(exprs(ff_pre))) {
    ff_pre@exprs <- cbind(ff_pre@exprs,
                          Original_ID = seq_len(nrow(ff_pre@exprs)))
  }
  p <- ggplot(df[i,], aes(x = x, y = y)) +
    geom_point(size = 0.5,
               color = ifelse(exprs(ff_pre)[i,"Original_ID"] %in%
                                exprs(ff_post)[,"Original_ID"], 'blue', 'red')) +
    xlab(GetMarkers(ff_pre, channel_x)) + 
    ylab(GetMarkers(ff_pre, channel_y)) +
    theme_minimal() + theme(legend.position = "none") +
    ggtitle(title)
  return(p)
}
to_plot <- list(list(ff_pre = ff,
                     ff_post = ff_m,
                     title = "Removed margin events",
                     channel_x = "PerCP-Cy5-5-A",
                     channel_y = "BV605-A"),
                list(ff_pre = ff_t,
                     ff_post = ff_s,
                     title = "Removed doublets",
                     channel_x = "FSC-A",
                     channel_y = "FSC-H"))

plot_list <- list()
for (plot in to_plot) {
  plot_list[[length(plot_list) + 1]] <- filter_plot(ff_pre = plot$ff_pre,
                                                    ff_post = plot$ff_post,
                                                    title = plot$title,
                                                    channel_x = plot$channel_x,
                                                    channel_y = plot$channel_y)
}

png(paste0(dir_QC, sub("fcs", "png", files[1])), width = 1920)
print(ggpubr::ggarrange(plotlist = plot_list, nrow = 1))
dev.off()

# 13. Run the preprocessing pipeline for all the files
for (file in files){
  ff <- read.FCS(paste0(dir_raw, file), truncate_max_range = FALSE)
  ff_m <- PeacoQC::RemoveMargins(ff, channels_of_interest)
  ff_c <- flowCore::compensate(ff_m, compensation_matrix)
  ff_t <- flowCore::transform(ff_c, translist)
  ff_s <- PeacoQC::RemoveDoublets(ff_t)
  #selected_live <- filter(ff_s, live_gate)
  #ff_l <- ff_s[selected_live@subSet, ]
  
  PQC <- PeacoQC::PeacoQC(ff = ff_s,
                          channels = channels_of_interest,
                          plot = TRUE, save_fcs = FALSE,
                          output_directory = dir_QC)
  
  write.FCS(PQC$FinalFF,
            file = paste0(dir_prepr, file))
}



###EFE DELETED PLOT CODE




# 14. Perform quality control between all files
# 14.(A) Plot the signal per channel and per file
# 14.(A)(i) Define the variables
files <- list.files(path = "C:/Users/edmondsonef/Desktop/FACS/FLOWSET/1-Preprocessed/1-05Jan2022/", pattern = "Samples")
files
dir_new <- "C:/Users/edmondsonef/Desktop/FACS/FLOWSET/1-Preprocessed/1-05Jan2022/"
#file_names <- sub(".*15_(.*).fcs", "\\1", files)
file_groups <- data$Group

# 14.(A)(ii) Make the overview plot
PlotFileScatters(input = paste0(dir_new, files),
                 channels = channels_of_interest,
                 #names = file_names, 
                 legend = TRUE,
                 groups = file_groups, nrow = 3,
                 plotFile = paste0(dir_QC, "file_scatters.png"))

# 14.(B) Perform principal component analysis (PCA)
# 14.(B)(i) Retrieve the median marker expression values per file
medians <- matrix(data = NA,
                  nrow = length(files), ncol = length(channels_of_interest),
                  dimnames = list(files, channels_of_interest))

for (file in files){
  ff <- read.FCS(paste0(dir_new, file))
  medians[file,] <- apply(exprs(ff)[,channels_of_interest], 2, median)
}

# 14.(B)(ii) Calculate the PCs
pc <- prcomp(medians, scale. = TRUE)

# 14.(B)(iii) Visualize the PCs
ggplot(data.frame(pc$x[,1:2], file_groups)) + 
  geom_point(aes(x= PC1, y = PC2, col = file_groups)) +
  theme_minimal()
ggsave(paste0(dir_QC, "file_PCA.png"), width = 5)

#}, times = 10)

#### Create an aggregate file ##################################################

########MOVE TO EARLIER FOR SUBSETTING
########MOVE TO EARLIER FOR SUBSETTING
########MOVE TO EARLIER FOR SUBSETTING
########MOVE TO EARLIER FOR SUBSETTING
########MOVE TO EARLIER FOR SUBSETTING
########MOVE TO EARLIER FOR SUBSETTING
########MOVE TO EARLIER FOR SUBSETTING
wsp_file <- 'C:/Users/edmondsonef/Desktop/Humanized/Flow/5-02Mar2022/15719 02Mar2022 Simone.wsp'
ws <- open_flowjo_xml(wsp_file)
ws
gs <- flowjo_to_gatingset(ws, name = 4, path ="C:/Users/edmondsonef/Desktop/FACS/")
plot(gs)


gs_get_pop_paths(gs)
recompute(gs)
gs_get_pop_paths(gs, path = "auto")
recompute(gs)


fj_ws_get_samples(ws, group_id = 4)
gs <- flowjo_to_gatingset(ws, name = 4, path=dir_raw)
gh_pop_get_stats(gs[[1]], "/scatter/sing")


cs <- gs_pop_get_data(gs, "/scatter/sing")
fs <- cytoset_to_flowSet(cs)

fs1 <- gh_pop_get_data(gs[[1]], "hCD45+")
fs2 <- gh_pop_get_data(gs[[2]], "hCD45+")
fs3 <- gh_pop_get_data(gs[[3]], "hCD45+")
fs4 <- gh_pop_get_data(gs[[4]], "hCD45+")
fs5 <- gh_pop_get_data(gs[[5]], "hCD45+")
fs6 <- gh_pop_get_data(gs[[6]], "hCD45+")
fs7 <- gh_pop_get_data(gs[[7]], "hCD45+")
fs8 <- gh_pop_get_data(gs[[8]], "hCD45+")


fs1 <- cytoframe_to_flowFrame(fs1)
fs2 <- cytoframe_to_flowFrame(fs2)
fs3 <- cytoframe_to_flowFrame(fs3)
fs4 <- cytoframe_to_flowFrame(fs4)
fs5 <- cytoframe_to_flowFrame(fs5)
fs6 <- cytoframe_to_flowFrame(fs6)
fs7 <- cytoframe_to_flowFrame(fs7)
fs8 <- cytoframe_to_flowFrame(fs8)
class(FS)

########MOVE TO EARLIER FOR SUBSETTING
########MOVE TO EARLIER FOR SUBSETTING
########MOVE TO EARLIER FOR SUBSETTING
########MOVE TO EARLIER FOR SUBSETTING
########MOVE TO EARLIER FOR SUBSETTING
########MOVE TO EARLIER FOR SUBSETTING
########MOVE TO EARLIER FOR SUBSETTING


# 15. Choose the number of cells to include in the aggregate file
n <- 700000

# 16. Make an aggregate file
set.seed(2020)

agg <- AggregateFlowFrames(c(fs1, fs2, fs3, fs4, fs5, fs6, fs7, fs8),
                           cTotal = n,
                           writeOutput = FALSE)
#                           outputFile = paste0(dir_prepr, "hCD45aggregateEFE.fcs"))
#}, times = 10)

#### Train FlowSOM model #######################################################
#microbenchmark::microbenchmark({

# 17. Specify the FlowSOM variables
SOM_x <- 10
SOM_y <- 10
n_meta <- 10
seed <- 2020
scaling <- FALSE


names(agg)
exprs(agg)
colnames(agg)
markers_of_interest <- c("SSC-A", "CD8", "CD4", "CD3", "CD33",
                         "CD11b", "CD25", "mCD45", "huCD45", "CD56", 
                         "CD19", "CD66b")
markers_of_interest <- c("SSC-A", "CD4", "CD3", "CD33",
                         "CD11b", "CD25", "mCD45", "huCD45", "CD56", 
                         "CD19", "CD66b")
markers_of_interest <- c("BB515-A",
                         "BB700-P-A",
                         "APC-A",
                         "APC-Cy7-A",
                         "BV421-A",
                         "BV786-A",
                         "PE-A",
                         "PE-CF594-A",
                         "PE-Cy7-A")

markers_of_interest <- c("Comp-BB515-A",
                         "Comp-BB700-P-A",
                         "Comp-APC-A",
                         "Comp-APC-Cy7-A",
                         "Comp-BV421-A",
                         "Comp-BV786-A",
                         "Comp-PE-A",
                         "Comp-PE-CF594-A",
                         "Comp-PE-Cy7-A")

# 18. Compute the FlowSOM object
fsom <- FlowSOM(input = cs,
                scale = scaling,
                colsToUse = c(7:12,15:17),
                seed = seed,
                nClus = n_meta,
                xdim = SOM_x, ydim = SOM_y)
saveRDS(fsom, paste(dir_RDS, "JANfsomEFE.rds"))

# 19. Visualize the FlowSOM object
PlotStars(fsom = fsom,
          backgroundValues = fsom$metaclustering)
ggsave(paste0(dir_results, "fsom_tree.pdf"),height = 8.5, width = 11)



#}, times = 10)

#### Test quality ##############################################################
#microbenchmark::microbenchmark({

# 20. Check the FlowSOM quality
# 20.(A) Make 2D scatter plots
# 20.(A)(i) Specify the parameters
channel_pairs = list(c("CD19", "SSC-A"),
                     c("CD3", "CD161"),
                     c("CD64", "CD49b"),
                     c("CD11c", "MHCII"),
                     c("Ly-6G", "CD11b"))
metaclusters_of_interest <- seq_len(n_meta)
clusters_of_interest <- NULL

# 20.(A)(ii) Make the 2D scatter plots
Plot2DScatters(fsom = fsom,
               channelpairs = channel_pairs,
               metaclusters = metaclusters_of_interest,
               clusters = clusters_of_interest,
               plotFile = paste0(dir_results, "fsom_2D_scatters.png"))





# 20.(B) Check the consistency with manual labeling
# 20.(B)(i) Extract the gating information from the wsp file
gating <- GetFlowJoLabels(files = files,
                          wspFile = wsp_file,
                          path = dir_raw)
names(gating)
head(gating)
# 20.(B)(ii) Get an overview of the gatenames and define the cell types of interest
new <- GetChannels(fs[[1]])


cell_types_of_interest <- c("Q6: CD3+ , CD4 [PCP55]+",
                            "Q10: CD3+ , CD8 [FITC]+",
                            "Q13: CD3- , CD19 [AFire750]+",
                            "Q17: CD3- , CD56+",
                            "Q18: CD3+ , CD56+",
                            "Q29: CD66b [PEDazz]- , CD11b [AF647]+",
                            "Q30: CD66b [PEDazz]+ , CD11b [AF647]+",
                            "Q31: CD66b [PEDazz]+ , CD11b [AF647]-",
                            "Q33: CD33- , CD11b [AF647]+",
                            "Q38: CD25+ , CD3+",
                            "Q35: CD33+ , CD11b [AF647]-",
                            "Q39: CD25+ , CD3-")

# 20.(B)(iii) Compile the labels of the aggregate file
aggregate_labels <- c()
for (file in unique(exprs(agg)[, "File"])) {
  aggregate_labels <- c(aggregate_labels, 
                        as.character(ManualVector(gating[[file]][["matrix"]],
                                                  cell_types_of_interest)
                                     [exprs(agg)[, "Original_ID"]
                                       [exprs(agg)[, "File"] == file]]))
}

# 20.(B)(iv) Show the manual labeling on the FlowSOM tree
PlotPies(fsom = fsom,
         cellTypes = factor(aggregate_labels, levels = c(cell_types_of_interest)))
ggsave(paste0(dir_QC, "fsom_manual.pdf"))

# 19.(B)(v) Calculate the purity of the FlowSOM clustering
Purity(realClusters = aggregate_labels,
       predictedClusters = GetClusters(fsom))

# 20.(C) Inspect the file contribution per cluster
# 20.(C)(i) Specify a color vector (optional)
file_colors <- c("#990000", "#cc0000", "#ff0000","#4677c7","#4677c7", #Different shades within the groups
                 "#1d1d77", "#2b3b92", "#3859ac", "#4677c7") 

# 20.(C)(ii) Show the file contribution
p <- PlotPies(fsom = fsom,
              cellTypes = factor(files[fsom$data[,"File"]]),
              colorPalette = file_colors)
AddStarsPies(p = p, # Legend to show how it should be
             arcs = data.frame(
               x0 = rep(0, length(files)),
               y0 = rep(0, length(files)),
               start = seq(0, 2 * pi, length.out = 8)[-8],
               end = seq(0, 2 * pi, length.out = 8)[-1],
               value = rep(1, length(files)),
               Markers = files),
             colorPalette = file_colors)
ggsave(paste0(dir_results, "fsom_filecontribution.pdf"))

#}, times = 10)

#### Discovery and downstream analysis #########################################
#microbenchmark::microbenchmark({

# 21. Explore the FlowSOM result
# 21.(A) Create the FlowSOMmary
FlowSOMmary(fsom = fsom,
            plotFile = paste0(dir_results, "fsom_summary.pdf"))

# 21.(B) Look for nodes with a specific pattern
# 21.(B)(i) Specify the query
query <- list("B cells" = c("CD19" = "high", "CD3" = "low"),
              "NK cells" = c("CD19" = "low", "CD161" = "high", 
                             "MHCII" = "low"),
              "T cells" = c("CD3" = "high", "MHCII" = "low", "Ly-6G" = "low"),
              "Macrophages" = c("CD64" = "high", "FcERI" = "high", 
                                "MHCII" = "high", "CD49b" = "high", 
                                "Ly-6G" = "low"),
              "Dendritic cells" = c("CD11c" = "high", "MHCII" = "high", 
                                    "CD11b" = "high", "FcERI" = "low"),
              "Neutrophils" = c("Ly-6G" = "high", "CD11b" = "high", 
                                "CD3" = "low"))

# 21.(B)(ii) Retrieve the cluster labels based on the query
labels <- QueryMultiple(fsom = fsom,
                        cellTypes = query,
                        plotFile = paste0(dir_results, "fsom_QueryStarPlot.pdf"))

# 21.(B)(iii) Show the retrieved labels on the FlowSOM tree
PlotVariable(fsom = fsom,
             variable = labels)
ggsave(paste0(dir_results, "fsom_query.pdf"))

# 22. Get features per fcs file
# Specify the variables of interest
types <- c("counts", "percentages", "MFIs")
MFIs <- c("CD49b", "Ly-6G")

# Get the features
features <- GetFeatures(fsom = fsom,
                        files = paste0(dir_prepr, files),
                        filenames = file_names,
                        type = types,
                        MFI = MFIs)

# 23. Define the groups and feature you would want to compare.
feature <- "cluster_percentages"
grouplist <- list("KO" = file_names[1:3],
                  "WT" = file_names[4:7])
stat <- "fold changes"

# 24. Compare the 2 groups of interest
stats <- GroupStats(features = features[[feature]],
                    groups = grouplist)

# 25. Show the findings of step 24 on the trees
# Define the plotting variables
stat_levels <- c(paste0(names(grouplist)[2], " underrepresented compared to ",
                        names(grouplist)[1]),
                 paste0(names(grouplist)[1], " underrepresented compared to ",
                        names(grouplist)[2]),
                 "--")
colors <- c("blue", "red", "white")

# Show statistical findings on FlowSOM trees
cluster_stat <- stats[stat,]
cluster_stat <- factor(ifelse(cluster_stat < -2.5, stat_levels[1],
                              ifelse(cluster_stat > 2.5, stat_levels[2],
                                     stat_levels[3])), 
                       levels = stat_levels)
cluster_stat[is.na(cluster_stat)] <- stat_levels[3]
gr_1 <- PlotStars(fsom = fsom, title = names(grouplist)[1], 
                  nodeSizes = stats[paste0("medians ", names(grouplist)[1]),], 
                  backgroundValues = cluster_stat,
                  backgroundColors = colors, 
                  list_insteadof_ggarrange = TRUE)
gr_2 <- PlotStars(fsom = fsom, title = names(grouplist)[2], 
                  nodeSizes = stats[paste0("medians ", names(grouplist)[2]),],
                  backgroundValues = cluster_stat,
                  backgroundColors = colors,
                  list_insteadof_ggarrange = TRUE)
ggpubr::ggarrange(plotlist = list(gr_1$tree, gr_2$tree, gr_2$starLegend, 
                                  gr_2$backgroundLegend), 
                  heights = c(3,1))
ggsave(paste0(dir_results, "fsom_groups.pdf"), width = 10, height = 7.5)


# 26. Map new data on the FlowSOM object
for (file in files){
  ff_prepr <- read.FCS(paste0(dir_prepr, file))
  ff_raw <- read.FCS(paste0(dir_raw, file))
  fsom_tmp <- NewData(fsom = fsom,
                      input = ff_prepr)
  clustering <- GetClusters(fsom_tmp)
  clustering_raw <- matrix(data = rep(0, nrow(exprs(ff_raw))),
                           ncol = 1, dimnames = list(c(), "FlowSOM"))
  clustering_raw[exprs(ff_prepr)[,"Original_ID"]] <- clustering
  ff_tmp <- flowCore::fr_append_cols(ff_raw, clustering_raw)
  write.FCS(ff_tmp, paste0(dir_prepr, "FlowSOM_", file))
}

#}, times = 10)

#### Additional FlowSOM approaches #############################################
#microbenchmark::microbenchmark({

### Applying FlowSOM to files or groups separately and then meta-cluster on all ####
# Compute separate FlowSOM objects
fsom_KO <- FlowSOM(input = paste0(dir_prepr, files[1:3]),
                   scale = FALSE, colsToUse = channels_of_interest,
                   seed = 2020)

fsom_WT <- FlowSOM(input = paste0(dir_prepr, files[4:7]),
                   scale = FALSE, colsToUse = channels_of_interest,
                   seed = 2020)

# Extract the cluster median fluorescence intensity values (MFIs)
MFI_KO <- GetClusterMFIs(fsom = fsom_KO, prettyColnames = TRUE, colsUsed = TRUE)
rownames(MFI_KO) <- paste0("KO", rownames(MFI_KO))
MFI_WT <- GetClusterMFIs(fsom = fsom_WT, prettyColnames = TRUE, colsUsed = TRUE)
rownames(MFI_WT) <- paste0("WT", rownames(MFI_WT))

# Obtain the meta-clusters by hierarchical clustering
all_clusters <- rbind(MFI_KO, MFI_WT)
hclust <- hclust(dist(all_clusters))
metaclustering <- cutree(hclust, 15) #MC 14 corresponds to the NK cells

# Generate one clustering heatmap from all clusters
ann <- data.frame(cohort = rep(c("KO", "WT"), each = 100), 
                  row.names = rownames(all_clusters))
p <- pheatmap::pheatmap(t(all_clusters), cluster_rows = F, cutree_cols = 15, 
                        cellwidth = 5, fontsize_col = 3, annotation_col = ann,
                        cluster_cols = hclust)
ggsave(p, filename = paste0(dir_results, "Higher_level_clustering.pdf"), width = 17)

# Generate the meta-cluster percentages boxplots
fsom_KO$metaclustering <- factor(unname(metaclustering[1:100]), levels = 1:15)
fsom_WT$metaclustering <- factor(unname(metaclustering[101:200]), levels =  1:15)
perc_KO <- GetFeatures(fsom = fsom_KO, 
                       files = paste0(dir_prepr, files[1:3]),
                       level = "metaclusters", type = "percentages", 
                       filenames = files[1:3])
perc_WT <- GetFeatures(fsom = fsom_WT, 
                       files = paste0(dir_prepr, files[4:7]),
                       level = "metaclusters", type = "percentages", 
                       filenames = files[4:7])

df <- data.frame(rbind(perc_KO[[1]], perc_WT[[1]])*100, 
                 cohort = rep(c("KO", "WT"), c(3, 4)), check.names = FALSE)
df_g <- tidyr::gather(df, "MC", "percentage", -cohort)
ggplot(df_g, aes(x = cohort, y = percentage)) +
  geom_boxplot() +
  facet_wrap(~MC, scales = "free") +
  theme_minimal()
ggsave(filename = "Results/FlowSOM_boxplot.pdf", width = 10, height = 10)

#}, times = 10)

## Hierarchical approach #######################################################
#microbenchmark::microbenchmark({

# Read in preprocessed fcs file, lymphocyte panel
ff <- read.FCS(paste0(dir_raw, "lympho.fcs"))
manual_labels <- readRDS(paste0(dir_raw, "attachments/lympho_labels.rds"))

# Perform a first level clustering to isolate the lymphocytes
fsom_level1 <- FlowSOM(input = ff,
                       scale = FALSE,
                       colsToUse = c("CD11b", "CD3", "CD161", "CD19"),
                       seed = 2020)

# Inspect the 2D scatter plots to identify the meta-clusters of interest
Plot2DScatters(fsom = fsom_level1, 
               channelpairs = list(c("CD3", "CD161")),
               metaclusters = 1:10, 
               plotFile = paste0(dir_results, "hierarchy_level1.png")) 
#MC 1, 4 and 5 are the lymphocytes (CD3+, CD161-)

# Subset the original fcs file
fsom_tmp <- NewData(fsom_level1, ff)
clustering <- GetMetaclusters(fsom_tmp)
ff_tmp <- ff[clustering %in% c(1, 4, 5),]

# Perform a second level clustering to characterize the lymphocytes
fsom_level2 <- FlowSOM(input = ff_tmp,
                       scale = FALSE,
                       colsToUse = c("TCRyd", "CD44", "CD4", "CD62L", "CD8"),
                       seed = 2020)

# Plot the lymphocytes FlowSOM tree
PlotStars(fsom = fsom_level2,
          backgroundValues = fsom_level2$metaclustering)

# Show the manual labels on the FlowSOM trees
PlotPies(fsom = fsom_level1, cellTypes = manual_labels,
         title = "First level clustering")
PlotPies(fsom = fsom_level2, cellTypes = factor(manual_labels[clustering %in% c(1, 4, 5)]),
         backgroundValues = fsom_level2$metaclustering, title = "Second level clustering")

#}, times = 10)







PlotDimRed(fsom, colsToUse = fsom$map$colsUsed, colorBy = "metaclusters")


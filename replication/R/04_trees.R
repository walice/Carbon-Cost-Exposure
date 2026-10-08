# 04_trees.R
# Step 4: classification tree (Figure 2), 1,000-split simulation, random forests
# and variable-importance plots (Figure 3).
# Input : data/derived/panel_imputed.rds
# Output: output/figures/classification_tree.pdf (Figure 2),
#         varimp_oppose.png + varimp_support.png (Figure 3),
#         output/tables/tree_accuracy.txt, tree_simulation_1000.csv

source(here::here("R", "00_setup.R"))
panel <- readRDS(here("data", "derived", "panel_imputed.rds"))
wave7IDs <- panel %>% filter(wave == "wave7") %>% pull(responseid)
sample <- panel %>% filter(responseid %in% wave7IDs)

# CLASSIFICATION TREES      ####
## ## ## ## ## ## ## ## ## ## ##

# .. Prep the data ####
vars <- c(
  "cp_oppose",
  "bachelors",
  "income_num_mid",
  "rural",
  # "left_right_num",
  "conservative",
  # "inc_heat_perceived_num",
  "inc_gas_perceived_num",
  "inc_overall_perceived_num",
  "gasprice_change_perceived_num",
  "owner",
  # "home_size_num",
  "fossil_home",
  "fossil_water",
  "fossil_stove",
  "drive",
  "vehicle_num",
  # "km_driven_num",
  "bill_diesel_num",
  "bill_elec_num"
)

# Subset data to obtain complete cases for the variables in the model
sample_complete <- sample %>%
  select(all_of(vars)) %>%
  drop_na

# Data manip
sample_complete <- sample_complete %>%
  mutate_at(vars(bachelors, rural, conservative,
                 owner, fossil_home, fossil_water, fossil_stove,
                 drive),
            ~bin_to_num(.))

levels(sample_complete$cp_oppose) <- c("Support", "Oppose")


# .. Randomly split data into training and test sets ####
set.seed(1509)
test.indices <- sample(1:nrow(sample_complete), round(nrow(sample_complete)*0.2))
sample.train <- sample_complete[-test.indices, ]
sample.test <- sample_complete[test.indices, ]


# .. Use the tree package ####
# Set tree controls
tree.opts <- tree.control(nrow(sample.train), 
                          minsize = 5, 
                          mindev = 1e-5)

# Grow the tree
set.seed(1509)
cp_oppose.tree <- tree(cp_oppose ~ . -cp_oppose,
                       data = sample.train, 
                       control = tree.opts)

# Perform cost-complexity pruning
cp_oppose.tree.10 <- prune.tree(cp_oppose.tree, 
                                best = 10)

# Draw tree with tree package
# dev.off()
# Tree grown with the full current feature list (incl. bill variables). Written
# straight to PDF (the working script drew it to the RStudio device).
pdf(here("output", "figures", "classification_tree_with_bills.pdf"), width = 11, height = 8)
draw.tree(cp_oppose.tree.10, 
          nodeinfo = TRUE,
          print.levels = TRUE,
          cex = 0.7)
dev.off()

# .. Calculate final out of sample predictive accuracy

# Make predictions on the test set
cp_oppose.pred <- predict(cp_oppose.tree.10, 
                          newdata = sample.test, 
                          type = "class")

# Calculate accuracy
accuracy <- mean(cp_oppose.pred == sample.test$cp_oppose)

# Print results
round(accuracy * 100, 2)
writeLines(sprintf("Tree with bill variables: N = %d, training = %d, leaves = %d, out-of-sample accuracy = %.2f%% (the figure quoted in the Figure 2 caption), majority-class baseline (test set) = %.1f%%",
                   nrow(sample_complete), nrow(sample.train), sum(cp_oppose.tree.10$frame$var == "<leaf>"),
                   accuracy * 100, 100 * max(prop.table(table(sample.test$cp_oppose)))),
           here("output", "tables", "tree_accuracy.txt"))



# .. Published Figure 2: tree grown WITHOUT the monthly bill variables ####
# The tree shown as Figure 2 in the published article was grown before
# bill_diesel_num and bill_elec_num were added to the feature list (commit
# 7ad2311 of the working repository). With those two variables excluded the
# complete-case sample is 882 respondents (706 training / 176 test), the pruned
# tree has exactly 10 leaves, and the splits, node counts and training accuracy
# (74.1%) match the published figure. The 69.48% out-of-sample accuracy quoted
# in the Figure 2 caption comes from the tree grown WITH the bill variables
# (classification_tree_with_bills.pdf above, 14 leaves after pruning).
vars_fig2 <- setdiff(vars, c("bill_diesel_num", "bill_elec_num"))
sample_fig2 <- sample %>%
  select(all_of(vars_fig2)) %>%
  drop_na %>%
  mutate_at(vars(bachelors, rural, conservative,
                 owner, fossil_home, fossil_water, fossil_stove,
                 drive),
            ~bin_to_num(.))
levels(sample_fig2$cp_oppose) <- c("Support", "Oppose")
set.seed(1509)
test.indices.fig2 <- sample(1:nrow(sample_fig2), round(nrow(sample_fig2)*0.2))
train.fig2 <- sample_fig2[-test.indices.fig2, ]
test.fig2  <- sample_fig2[test.indices.fig2, ]
set.seed(1509)
tree.fig2 <- tree(cp_oppose ~ . -cp_oppose, data = train.fig2,
                  control = tree.control(nrow(train.fig2), minsize = 5, mindev = 1e-5))
tree.fig2.10 <- prune.tree(tree.fig2, best = 10)
pdf(here("output", "figures", "classification_tree.pdf"), width = 11, height = 8)
draw.tree(tree.fig2.10, nodeinfo = TRUE, print.levels = TRUE, cex = 0.7)
dev.off()
acc.fig2 <- mean(predict(tree.fig2.10, newdata = test.fig2, type = "class") == test.fig2$cp_oppose)
cat(sprintf("Figure 2 tree (no bill variables): N = %d, training = %d, leaves = %d, out-of-sample accuracy = %.2f%%, majority-class baseline (test set) = %.1f%%\n",
            nrow(sample_fig2), nrow(train.fig2), sum(tree.fig2.10$frame$var == "<leaf>"),
            100 * acc.fig2, 100 * max(prop.table(table(test.fig2$cp_oppose)))),
    file = here("output", "tables", "tree_accuracy.txt"), append = TRUE)


# .. Simulate 1,000 trees to obtain most important variable ####
# Set up records to collect top variable
nsims <- 1000
records <- matrix(NA, ncol = 6, nrow = nsims)
colnames(records) <- c("simulation", "variable", 
                       "2nd var", "misclass", "used", "size")
indices <- matrix(NA, ncol = nsims, nrow = nrow(sample.test))

set.seed(1509)
i <- 1
for (i in 1:nsims){
  test.indices <- sample(1:nrow(sample_complete), round(nrow(sample_complete)*0.2))
  sample.train <- sample_complete[-test.indices, ]
  sample.test <- sample_complete[test.indices, ]
  indices[, i] <- test.indices
  
  tree <- NULL
  tree <- tree(cp_oppose ~ . -cp_oppose,
               data = sample.train, 
               control = tree.opts)
  tree.10 <- prune.tree(tree, 
                        best = 10)
  records[i, 1] <- i
  records[i, 2] <- as.character(tree.10$frame[1, "var"])
  records[i, 3] <- as.character(tree.10$frame[4, "var"])
  records[i, 4] <- summary(tree.10)$misclass[1] / summary(tree.10)$misclass[2]
  records[i, 5] <- length(summary(tree.10)$used)
  records[i, 6] <- summary(tree.10)$size
}

# Simulation with the lowest misclassification rate
sim <- which(records[, "misclass"] == min(records[, "misclass"]))
ind <- indices[, sim]
sample.train <- sample_complete[-ind, ]
sample.test <- sample_complete[ind, ]
tree.opts <- tree.control(nrow(sample.train), 
                          minsize = 5, 
                          mindev = 1e-5)
cp_oppose.tree <- tree(cp_oppose ~ . -cp_oppose,
                       data = sample.train, 
                       control = tree.opts)
cp_oppose.tree.10 <- prune.tree(cp_oppose.tree, 
                                best = 10)
# Tree from the best of 1,000 random train/test splits (not shown in the paper)
pdf(here("output", "figures", "classification_tree_best_of_1000.pdf"), width = 11, height = 8)
draw.tree(cp_oppose.tree.10, 
          nodeinfo = TRUE,
          print.levels = TRUE,
          cex = 0.5)
dev.off()
write.csv(as.data.frame(records), here("output", "tables", "tree_simulation_1000.csv"), row.names = FALSE)

# .. Random forest approach ####
# Opposition to carbon pricing
set.seed(1509)
cp_oppose.rf <- randomForest(cp_oppose ~ . -cp_oppose,
                             data = sample_complete, 
                             importance = TRUE,
                             ntree = 1000)
cp_oppose.rf
cp_oppose.varimp <- varImp(cp_oppose.rf)
cp_oppose.varimp$var <- rownames(cp_oppose.varimp)
varImpPlot(cp_oppose.rf, cex = 0.7)

# Support for carbon pricing
vars <- vars[-which(vars == "cp_oppose" | vars == "conservative")]
vars <- c(vars, c("cp_support", "liberal"))

sample_complete <- sample %>%
  select(all_of(vars)) %>%
  drop_na

set.seed(1509)
cp_support.rf <- randomForest(cp_support ~ . -cp_support,
                              data = sample_complete, 
                              importance = TRUE,
                              ntree = 1000)
cp_support.rf
cp_support.varimp <- varImp(cp_support.rf)
cp_support.varimp$var <- rownames(cp_support.varimp)
varImpPlot(cp_support.rf, cex = 0.7)


# .. Create variable importance plot ####

# Clean variable name labels for figures
varimp_labels <- c(
  conservative              = "Conservative voter",
  liberal                   = "Liberal voter",
  inc_overall_perceived_num = "Perc. overall energy cost increase",
  inc_gas_perceived_num     = "Perc. gas cost increase ($/mo)",
  gasprice_change_perceived_num = "Perc. gas price change (cents/L)",
  fossil_home               = "Home heating: fossil fuels",
  fossil_water              = "Water heating: fossil fuels",
  fossil_stove              = "Fossil fuel stove",
  bill_diesel_num           = "Monthly gasoline/diesel bill",
  bill_elec_num             = "Monthly electricity bill",
  vehicle_num               = "Number of vehicles owned",
  drive                     = "Drives to work",
  rural                     = "Rural residence",
  bachelors                 = "Bachelor's degree or higher",
  owner                     = "Home owner",
  income_num_mid            = "Household income (midpoint)",
  home_size_num             = "Home size (sq. ft.)"
)

# Helper: apply labels, leaving any unmapped vars as-is
apply_labels <- function(df) {
  df$var_label <- ifelse(df$var %in% names(varimp_labels),
                         varimp_labels[df$var],
                         df$var)
  df
}

# Oppose
g <- ggplot(cp_oppose.varimp %>%
              select(var, Oppose) %>%
              apply_labels(),
            aes(x = fct_reorder(var_label, Oppose), y = Oppose)) +
  geom_segment(aes(xend = var_label, y = 0, yend = Oppose), color = "#00B0F6") +
  geom_point(size = 4, color = "#00B0F6") +
  theme_minimal(base_size = 12) +
  theme(axis.text.y = element_text(size = 11)) +
  coord_flip() +
  labs(title = "Variable importance",
       subtitle = "Predicting opposition to carbon pricing",
       x = "", y = "Mean decrease in accuracy")
ggsave(g,
       file = here("output", "figures", "varimp_oppose.png"),
       width = 7, height = 5, units = "in")

# Support. caret::varImp() names its columns after the outcome's factor levels;
# for cp_support those are "0"/"1", so take the column for level "1" (= supports).
imp_col <- if ("Support" %in% names(cp_support.varimp)) "Support" else
  setdiff(names(cp_support.varimp), "var")[2]
cp_support.varimp <- cp_support.varimp %>% mutate(imp = .data[[imp_col]])
g <- ggplot(cp_support.varimp %>%
              select(var, imp) %>%
              apply_labels(),
            aes(x = fct_reorder(var_label, imp), y = imp)) +
  geom_segment(aes(xend = var_label, y = 0, yend = imp), color = "#FFD84D") +
  geom_point(size = 4, color = "#FFD84D") +
  theme_minimal(base_size = 12) +
  theme(axis.text.y = element_text(size = 11)) +
  coord_flip() +
  labs(title = "Variable importance",
       subtitle = "Predicting support for carbon pricing",
       x = "", y = "Mean decrease in accuracy")
ggsave(g,
       file = here("output", "figures", "varimp_support.png"),
       width = 7, height = 5, units = "in")





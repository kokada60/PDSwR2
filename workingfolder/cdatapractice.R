tf <- wrapr::qchar_frame(
"d", "xa", "xb", "ya", "yb" |
  "1",  "1",  "3",  "6",  "8" |
  "2",  "2",  "4",  "7",  "9" 
) 
tf2 <- build_frame(
  "d", "xa", "xb", "ya", "yb" |
    "1",  "1",  "3",  "6",  "8" |
    "2",  "2",  "4",  "7",  "9" 
)

identical(tf, tf2)

irisDT <- iris %>% setDT()
library(cdata)
iris



d <- wrapr::build_frame(
  "id"  , "AUC", "R2" |
    1   , 0.7  , 0.4  |
    2   , 0.8  , 0.5  )


startingData <- wrapr::build_frame(
  "val_loss", "val_acc", "loss", "acc", "epoch" |
    0.3769818, 0.8722, 0.5067290, 0.7852000, 1 |
    0.2996994, 0.8895, 0.3002033, 0.9040000, 2 |
    0.2963943, 0.8822, 0.2165675, 0.9303333, 3 |
    0.2779052, 0.8899, 0.1738829, 0.9428000, 4 |
    0.2842501, 0.8861, 0.1410933, 0.9545333, 5 |
    0.3119754, 0.8817, 0.1135626, 0.9656000, 6
)
onerow <- startingData %.>% head(., 1)

control_table <- wrapr::build_frame(
  "measurements", "validation", "training" |
    "accuracy",      "val_acc",     "acc"       |
   "minus binary cross entropy", "val_loss", "loss"   
)
blockf <- rowrecs_to_blocks(
  startingData, control_table, columnsToCopy = "epoch"    
)
rowf <- blocks_to_rowrecs(
  blockf, control_table, keyColumns = "epoch"
)
cR = control_table %>%
  blocks_to_rowrecs(.,
    controlTable = .,
    # no record id column
    keyColumns = NULL) 
cI <- cR %>% rowrecs_to_blocks(., 
    controlTable = control_table)
dPlot <- startingData %.>% rowrecs_to_blocks(., controlTable = control_table, columnsToCopy = "epoch")


## Iris data
control_table <- build_frame(
  controlTable = control_markee,,
)
}
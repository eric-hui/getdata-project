#!/usr/bin/R
# ------------------------------------------------------------------------
# run_analysis.R
#    R script to load data from the "UCI HAR Dataset", and generate
#    summary statistics for the average of the "mean()" and "std()" of the
#    features.  The summary statistics are grouped by the SubjectId and
#    the Activity.
#
# Note: the script should be run from the "UCI HAR Dataset" directory as
# the working directory.
# ------------------------------------------------------------------------

# ------------------------------------------------------------------------
# load_dataset
#    Loads the "UCI HAR Dataset" data from the specified path.  In this
#    case, path should either be "test" or "train".  The data from
#    the subject_*.txt, X_*.txt, and y_*.txt files are column merged as
#    follows:
#
#    subject_*.txt: single integer column, renamed "SubjectId"
#    y_*.txt: single integer column, renamed "Activity" and converted from
#        integers into ascii string for the corresponding activity name.
#    X_*.txt: multi-column numerics (561 cols).  only relevant columns
#        extracted (mean and std), with corresponding column names.
#
#    Returns the subsetted data with the following columngs:
#      SubjectId: subject id, integer
#      Activity: activity name, string
#      66 cols: numeric feature values, see extractFeatures.txt for names
#          and ordering of the columns
# ------------------------------------------------------------------------
load_dataset <- function (path) {
    # read SubjectId
    fname       <- Sys.glob(file.path(path, 'subject_*.txt'))
    subject     <- read.table(fname[1], col.names = c('SubjectId'))

    # read X features
    fname       <- Sys.glob(file.path(path, 'X_*.txt'))
    feature     <- read.table(fname[1])
    
    # read Activity
    fname       <- Sys.glob(file.path(path, 'y_*.txt'))
    value       <- read.table(fname[1], col.names = c('ActivityType'))

    # convert ActivityType (ID) into Activity (name).
    # see: UCI HAR Dataset/activity_labels.txt for definitions
    c           <- factor(value$ActivityType)
    levels(c)   <- c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS",
                     "SITTING", "STANDING", "LAYING")
    
    # column merge data.  note: inline subsetting of the features
    d           <- cbind(subject, c, extract_features(feature))
    names(d)[2] <- "Activity"
    d
}

# ------------------------------------------------------------------------
# extract_features
#    Extracts pertinent features (mean and std) from the feature data
#    frame.  The feature data frame should be the unmodified Nx561 numeric
#    data frame loaded from the UCI HAR Dataset file.  Uses the global
#    myFeatures to extract relevant columns and name the columns.
# ------------------------------------------------------------------------
extract_features <- function (feature) {
    subfeatures <- feature[, myFeatures$V1]
    names(subfeatures) <- myFeatures$V2
    subfeatures
}

# ------------------------------------------------------------------------
# calc_avgs
#    Calculate the average of the feature variables fore each activity and
#    subject.  data is the subsetted data in the form returned from
#    load_dataset.  The aggregate averages are calculated for each
#    (SubjectId, Activity) pair present in the data.
#
#    Returns an Nx68 table with columns:
#        SubjectId: integer subject id
#        Activity: name of activity
#        66 cols: numeric average over all observations of feature.  The
#           column name is the same as the original feature name with
#           ".avg" appended.
# ------------------------------------------------------------------------
calc_avgs <- function (data) {
    avg <- aggregate(data, by = list(data$Activity, data$SubjectId), mean)

    # rename column and remove unused values
    avg$Activity <- avg$Group.1
    avg$Group.1 <- NULL
    avg$Group.2 <- NULL
    
    # rename columns to indicate quantities are averages
    n <- names(avg)
    for (i in 3:length(n)) {
        names(avg)[i] <- sprintf('%s.avg', n[i])
    }
    avg
}

# ------------------------------------------------------------------------
# main script
# ------------------------------------------------------------------------

# check to see if we want to use pre-cached intermediate data (for development
# and debugging)
useCache = FALSE
if (useCache) {
    all_data <- read.table("_cacheTable.txt", header = TRUE)
} else {
    # load metadata on which features we want to extract along with the
    # variable name.  myFeatures$V1 is the column number of the feature in
    # the X_*.txt file and myFeatures$V2 is the variable name.
    script.dir <- dirname(sys.frame(1)$ofile)
    myFeatures <- read.table(file.path(script.dir, "extractFeatures.txt"))
    
    # load and merge the test and train data sets (and save results to a
    # cached data file.
    test        <- load_dataset('test')
    train       <- load_dataset('train')
    all_data    <- rbind(test, train)
    write.table(all_data, "_cacheTable.txt", row.names = FALSE)
} 

# generate the average statistics and save to output data file
avg <- calc_avgs(all_data)
write.table(avg, "tidy_data.txt", row.name = FALSE)


Code Book
================
Hristo Iliev
2024-12-21

## Code book for the tidying of the

## Human Activity Recognition Using Smartphones (HARUS) Dataset

This script uses R and tidyverse to clean and wrangle the HARUS Dataset,
built from the recordings of 30 subjects performing activities of daily
living (ADL) while carrying a waist-mounted smartphone with embedded
inertial sensors.

More information can be found
[here](https://archive.ics.uci.edu/dataset/240/human+activity+recognition+using+smartphones).

First, the raw dataset is downloaded from the web, unzipped and stored
locally in a subfolder of the current working directory, called
`HARUSDataset`. This dataset contains the training and test sets, each
one in a separate subfolder, together with the variable names and
subject indeces. There are also explanatory files with the coding of
each variable and activity.

The data from the following files is then read by the `read.table`
function: - X_train.txt - y_train.txt - subject_train.txt - X_test.txt -
y_test.txt - subject_test.txt - features.txt

1.  The complete raw dataset is then assembled by binding the train and
    test sets, adding the names of the variables and the columns for the
    subject and activity identifiers.

2.  Using `dplyr` only the columns whith names containing the substrings
    “mean()” and “std” are selected.

3.  A new column is created, called `ActivityNames` with descriptive
    activity names according to the `activity_label.txt`.

4.  Variables are renamed with descriptive names, substituting:

- “t” with “time”
- “f” with “fourier”
- “Acc” with “Acceleration”

(*see* `features_info.txt`)

5.  The dataset is then grouped by activity name and subject and the
    mean value of every variable is calculated per activity and subject,
    resulting in the final tidy dataset.

It is then written to and saved as `dataTidy.csv` in the `HARUSDataset`
folder.

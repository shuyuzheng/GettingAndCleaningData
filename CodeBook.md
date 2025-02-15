Original data
-------------

The original data was curated in “Human Activity Recognition Using
Smartphones Dataset Version 1.0”. It was downloaded from [UC Irvine
Machine Learning Repository](http://archive.ics.uci.edu/ml/index.php)
and saved in the folder `UCI HAR Dataset`.

All measurements are floating-point values, normalised and bounded
within \[-1,1\].

Prior to normalisation, acceleration measurements (variables containing
Accelerometer) were made in g’s (9.81 m.s⁻²) and gyroscope measurements
(variables containing Gyroscope) were made in radians per second
(rad.s⁻¹).

Magnitudes of three-dimensional signals (variables containing Magnitude)
were calculated using the Euclidean norm.

A full description is available at the site where the data was obtained:
<a href="http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones" class="uri">http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones</a>

Process for cleaning data
-------------------------

1.  Merges the train set and the test sets to create one data set.
2.  Assembling the test data set (in folder `test`): match the measured
    value (in `X_test.txt`), with the label in (`y_test.txt`) and the
    subject (in `subject_test.txt`)
3.  Assembles the train data set: match the measured value (in
    `X_trian.txt`), with the label in (`y_trian.txt`) and the subject
    (in `subject_trian.txt`)
4.  Merges two data set by appending the train data set behind the test
    data set.

5.  Extracts only the measurements on the mean and standard deviation
    for each measurement.
6.  Matchs the data set with `features.txt` which contains the names of
    each variable.
7.  Using Regular expression `"mean\\(\\)|std\\(\\)"` selects the
    columns in data sets whose value indicate the mean or standard
    deviation of measurement.

8.  Uses descriptive activity names to name the activities in the data
    set. 1. Reading `activity_labels.txt` file into memory.
9.  Transform `label` column of data set into `factor` type with setting
    the `level` argument as the label column in `activity_labels` file
    and `label` argument as the description column in `activity_labels`
    file.

10. Appropriately labels the data set with descriptive variable names.
11. Reads `features_info.txt` file and understand the mean of each
    feature names.
12. Using completed expression substitutes the abbreviation in variable
    names to make it descriptive. Following is a list of substitution
    used in changing variable names:

<!-- -->

    | abbreviation | Substitution        |
    | ------------ | ------------        |
    | "^t"         | "TimeDomain"        |
    | "^f"         | "FrequencyDomain"   | 
    | "Mag"        | "Magnitude"         |
    | "Acc"        | "Accelormeter"      |
    | "Gyro"       | "Gyroscope"         |
    | "-mean"      | "MeanValue"         |
    | "-std"       | "StandardDeviation" |
    | "-X$"        | "X-axis"            |
    | "-Y$"        | "Y-axis"            |
    | "-Z$"        | "Z-axis"            |
    | "BodyBody"   | "Body"

1.  Generats a tidy data set with the average of each variable for each
    activity and each subject.
2.  Divids the data set into groups according to `label` and `subject`
    columns.
3.  Calculates the mean of all other columns.
4.  Wites the data set into file `tidy_data.txt`.

Structure of tidy data set
--------------------------

### Meaning of values

Each row contains the average of each variable (all mean and standard
deviation of measurements in the features) for each activity and each
subject.(numerical)

### identifiers

1.  `subject`(integer): the identifier of 30 volunteers who attended
    into this experiment.
2.  `activity`(character): the activities the volunteers were doing when
    data was collected. There are totally 6 different activities:

-   WALKING: subject was walking
-   WALKING\_UPSTAIRS: subject was walking upstairs
-   WALKING\_DOWNSTAIRS: subject was walking downstairs
-   SITTING: subject was sitting
-   STANDING: subject was standing
-   LAYING: subject was laying

### Variables

The variables are mean value and standard deviation estimated from raw
signals (captured by accelerometer or gyroscope from 3-axis directions).

The constructure of variable names is
`<domain><sensor><signal><metric><direction>`

-   `<domain>`: the domain of the signals
    -   “TimeDomain”: time domain signals Which were captured at a
        constant rate of 50 Hz.
    -   “FrequencyDomain”: fre domain signals detected by a Fast Fourier
        Transform (FFT).
-   `<sensors>`: the type of sensor used to collect signals.
    -   “Accelormeter”: a body accelormeter.
    -   “Gyroscope”: a gyroscope.
-   `<signal>`: indicates the type of signals which were directly
    collected by sensors or calculated from raw signal.
    -   "": the raw signal directly collected by sensors.
    -   “Jerk”: Jerk signal derived in time from the body linear
        acceleration and angular velocity.
    -   “Magnitude”: The magnitude of the three-dimensional Jerk signals
        calculated by using the Euclidean norm
-   `<metric>`: indicates the type of metric value calculated from
    signal.
    -   “StandardDeviation”: Standard deviation of the signals.
    -   “Mean”: Mean value of the signals.
-   `<direction>`: indicates along which axis the raw signal was
    generated.
    -   “X-axis”: the raw signal was generated along X-axis.
    -   “Y-axis”: the raw signal was generated along Y-axis.
    -   “Z-axis”: the raw signal was generated along Z-axis.

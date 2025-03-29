!> @brief
!! Holds all constant parameters used for reading in properties files.
!! Contains parameters for names of properties and for property options.
!!
!! @copyright
!! THIS SOFTWARE AND ITS DOCUMENTATION ARE CONSIDERED TO BE IN THE PUBLIC
!! DOMAIN AND THUS ARE AVAILABLE FOR UNRESTRICTED PUBLIC USE. THEY ARE
!! FURNISHED "AS IS." THE AUTHORS, THE UNITED STATES GOVERNMENT, ITS
!! INSTRUMENTALITIES, OFFICERS, EMPLOYEES, AND AGENTS MAKE NO WARRANTY,
!! EXPRESS OR IMPLIED, AS TO THE USEFULNESS OF THE SOFTWARE AND
!! DOCUMENTATION FOR ANY PURPOSE. THEY ASSUME NO RESPONSIBILITY (1) FOR
!! THE USE OF THE SOFTWARE AND DOCUMENTATION; OR (2) TO PROVIDE TECHNICAL
!! SUPPORT TO USERS.
!!
module PropertyParameters

  use PropertyReader

  implicit none

  ! General properties related to the specific run of the software.

  !> Software version indicator; used in directory structure and file names.
  character(len=*), parameter :: PROP_VERSION = "pha.version"
  !> The earliest year possibly present in the input data for any station.
  character(len=*), parameter :: PROP_BEGIN_YEAR = "pha.begin-year"
  !> The element being processed during a given run; tavg, tmin, tmax, tdtr
  character(len=*), parameter :: PROP_ELEMENT = "pha.element"
  !> The input data type, as indicated by directory and file name; tob or raw
  character(len=*), parameter :: PROP_INPUT_DATA_TYPE = "pha.input-data-type"
  !> The input data type for neighbors-by-correlation code, as indicated by directory and file name; tob or raw
  !! This is separated from PROP_INPUT_DATA_TYPE to match original version of PHA neighbors code.
  character(len=*), parameter :: PROP_NEIGH_INPUT_DATA_TYPE = "pha.neighbors.input-data-type"


  ! File path properties

  !> Location of the station metadata file
  character(len=*), parameter :: PROP_PATH_STATION_META = "pha.path.station-metadata"
  !> Location of the closest neighbors (by distance) file
  character(len=*), parameter :: PROP_PATH_NEIGH_DISTANCE = "pha.path.neighbors.distance"
  !> Location of the best neighbors (by distance then correlation) file created
  character(len=*), parameter :: PROP_PATH_NEIGH_CORRELATION = "pha.path.neighbors.correlation"
  !> Location of the best neighbors (by distance then correlation) file to be used as input
  character(len=*), parameter :: PROP_PATH_NEIGH_CORRELATION_IN = "pha.path.neighbors.correlation-in"
  !> Location of the input data files for all stations
  character(len=*), parameter :: PROP_PATH_ELEMENT_DATA_IN = "pha.path.station-element-data-in"
  !> Location of the input data files for all stations, used by the neighbors-by-correlation code
  character(len=*), parameter :: PROP_PATH_NEIGH_ELEMENT_DATA_IN = "pha.path.neighbors.station-element-data-in"
  !> Location of the output data files for all stations
  character(len=*), parameter :: PROP_PATH_ELEMENT_DATA_OUT = "pha.path.station-element-data-out"
  !> Location of the station history files
  character(len=*), parameter :: PROP_PATH_HISTORY = "pha.path.station-history"

  !> Array of all property names that represent file paths
  character(len=32), dimension(:), parameter :: PROPS_FILES(4) =  &
                                                 (/ PROP_PATH_STATION_META//"        ",  &
                                                    PROP_PATH_NEIGH_DISTANCE//"      ",  &
                                                    PROP_PATH_NEIGH_CORRELATION//"   ",  &
                                                    PROP_PATH_NEIGH_CORRELATION_IN /)

  !> Array of all property names that represent directories
  character(len=45), dimension(:), parameter :: PROPS_DIRECTORIES(4) =  &
                                                 (/ PROP_PATH_ELEMENT_DATA_IN//"          ", &
                                                    PROP_PATH_ELEMENT_DATA_OUT//"         ", &
                                                    PROP_PATH_NEIGH_ELEMENT_DATA_IN,         &
                                                    PROP_PATH_HISTORY//"                  " /)

  ! Benchmark properties for finding best neighbors

  !> The number of stations to consider as potential neighbors for each target station. See req. 4.1.
  character(len=*), parameter :: PROP_NEIGH_BUFFERED_LIMIT = "pha.neighbors.distance-neighbor-limit"
  !> The method of choosing best neighbors. See req. 4.2.
  character(len=*), parameter :: PROP_NEIGH_METHOD = "pha.neighbors.method"
  !> The minimum correlation coefficient with a target station to qualify as a neighbor. See req. 4.3.
  character(len=*), parameter :: PROP_NEIGH_MIN_COEFF = "pha.neighbors.min-coefficient"
  !> The number of neighbors with data covering a given time period to meet the coverage
  !! threshold. See req. 4.4.
  character(len=*), parameter :: PROP_NEIGH_MIN_COVERAGE = "pha.neighbors.min-station-coverage"
  !> The maximum final number of neighbors per target station. See req. 4.5.
  character(len=*), parameter :: PROP_NEIGH_FINAL_LIMIT = "pha.neighbors.final-neighbor-limit"

  ! Benchmark properties for finding changepoints.

  !> Option for use of metadata in station history files in determining timing of shifts. Options are
  !! don't use history, use history only, use history and found changepoints. See req. 4.6.
  character(len=*), parameter :: PROP_USE_HISTORY_FILES = "pha.use-history-files"
  !> SNHT significance threshold as a percentage. 1 = 97.5%; 5 = 95%; 10=90%. See req. 4.7.
  character(len=*), parameter :: PROP_SNHT_THRESHOLD = "pha.snht-threshold"
  !> The penalty function type used to determine the form of a change point. Options are
  !! bic, aic, or none. See req. 4.8.
  character(len=*), parameter :: PROP_BIC_PENALTY = "pha.bic-penalty"
  !> The confidence limit used to quantify the timing uncertainty of a change point (merging closely
  !! timed change points), as a percentage. Options are 90=90%, 92=92.5%, 95=95%. See req. 4.9.
  character(len=*), parameter :: PROP_AMPLOC_PERCENT = "pha.amploc-percent"
  !> The number of target-neighbor difference series with coincident change points
  !! required to implicate the target. See req. 4.10.
  character(len=*), parameter :: PROP_CONFIRM_MIN_NEIGHS = "pha.confirm"
  !> The minimum length of data period, in months, that can be adjusted. See req. 4.11.
  character(len=*), parameter :: PROP_ADJUST_MIN_LENGTH = "pha.adjust.min-length"
  !> The number of pairwise change point estimates required to determine the size of
  !! an adjustment. See req. 4.12.
  character(len=*), parameter :: PROP_ADJUST_MIN_NEIGHS = "pha.adjust.min-neighbors"
  !> Whether or not to test and remove outliers using the Tukey outlier test
  !! before calculating the size of the change point. See req. 4.13.
  character(len=*), parameter :: PROP_ADJUST_REMOVE_OUTLIERS = "pha.adjust.remove-outliers"
  !> The minimum number of months before and after a break in the difference series necessary
  !! to calculate the size of the target change point. See req. 4.14.
  character(len=*), parameter :: PROP_ADJUST_WINDOW = "pha.adjust.window"
  !> The outlier filtering method for the pairwise change point estimates. Options are
  !! Tukey variant, BIC, both, or none. See req. 4.15.
  character(len=*), parameter :: PROP_ADJUST_FILTER_METHOD = "pha.adjust.filter-method"
  !> The method used to determine the adjustment factor from multiple pairwise estimates.
  !! Options are median size, the average size, or inter-quartile range average. See req. 4.16.
  character(len=*), parameter :: PROP_ADJUST_ESTIMATE_METHOD = "pha.adjust.est-method"
  !> Whether or not to merge data segments when the change point adjustment size
  !! is statistically insignificant. See req. 4.17.
  character(len=*), parameter :: PROP_REMOVE_INSIGNIFICANT = "pha.remove-insignificant"

  ! Logger properties

  !> Base name of the logger output file (date will be appended to the filename).
  character(len=*), parameter :: PROP_LOG_FILENAME = "pha.logger.filename"
  !> Minimum logging level to write to file.
  character(len=*), parameter :: PROP_LOG_LEVEL = "pha.logger.level"
  !> Whether or not to also print to standard out.
  character(len=*), parameter :: PROP_LOG_STDOUT = "pha.logger.print-to-stdout"
  !> Whether or not to append a datestamp to the log filename.
  character(len=*), parameter :: PROP_LOG_DATESTAMP = "pha.logger.append-datestamp"
  !> Whether or not to roll over to a new log filename datestamp after midnight.
  character(len=*), parameter :: PROP_LOG_ROLLOVER = "pha.logger.rollover-datestamp"

  ! Options for properties that have specific limited possible options

  !> Maximum temperature option for PROP_ELEMENT, which element to run
  character(len=*), parameter :: OPT_ELEMENT_TMAX = "tmax"
  !> Minimum temperature option for PROP_ELEMENT
  character(len=*), parameter :: OPT_ELEMENT_TMIN = "tmin"
  !> Average temperature option for PROP_ELEMENT
  character(len=*), parameter :: OPT_ELEMENT_TAVG = "tavg"
  !> Diurnal range temperature option for PROP_ELEMENT
  character(len=*), parameter :: OPT_ELEMENT_TDTR = "tdtr"

  !> First differences option for PROP_NEIGHBORS_METHOD, how to choose neighbors
  character(len=*), parameter :: OPT_NEIGH_METHOD_1ST_DIFFS = "first-diffs"
  !> Distance only (no correlation) option for PROP_NEIGHBORS_METHOD, how to choose neighbors
  character(len=*), parameter :: OPT_NEIGH_METHOD_DISTANCE = "distance-only"
  !> Montly anomaly option for PROP_NEIGHBORS_METHOD, how to choose neighbors
  character(len=*), parameter :: OPT_NEIGH_METHOD_ANOMALY = "monthly-anomaly"

  !> Use history files (documented changepoints) and also detect undocumented changepoints
  !! option for PROP_USE_HISTORY_FILES
  integer, parameter :: OPT_HISTORY_USE = 1
  !> Ignore history files option and only use detection of undocumented changepoints option
  !! for PROP_USE_HISTORY_FILES
  integer, parameter :: OPT_HISTORY_IGNORE = 0
  !> USe history files only and don't detect undocumented changepoints option for PROP_USE_HISTORY_FILES
  integer, parameter :: OPT_HISTORY_ONLY = -1

  !> 97.5% option for PROP_SNHT_THRESHOLD, the SNHT significance threshold percentage
  integer, parameter :: OPT_SNHT_97_5 = 1
  !> 95% option for PROP_SNHT_THRESHOLD, the SNHT significance threshold percentage
  integer, parameter :: OPT_SNHT_95 = 5
  !> 90% option for PROP_SNHT_THRESHOLD, the SNHT significance threshold percentage
  integer, parameter :: OPT_SNHT_90 = 10

  !> Bayesian Information Criterian (BIC) option for PROP_BIC_PENALTY, the penalty function type
  character(len=*), parameter :: OPT_PENALTY_BIC = "bic"
  !> Akaika Information Criterian (AIC) option for PROP_BIC_PENALTY, the penalty function type
  character(len=*), parameter :: OPT_PENALTY_AIC = "aic"
  !> "None" option for PROP_BIC_PENALTY, the penalty function type
  character(len=*), parameter :: OPT_PENALTY_NONE = "none"

  !> 90% option for PROP_AMPLOC_PERCENT, the confidence limit for the timing uncertainty
  integer, parameter :: OPT_AMPLOC_PERCENT_90 = 90
  !> 92.5% option for PROP_AMPLOC_PERCENT, the confidence limit for the timing uncertainty
  integer, parameter :: OPT_AMPLOC_PERCENT_92_5 = 92
  !> 95% option for PROP_AMPLOC_PERCENT, the confidence limit for the timing uncertainty
  integer, parameter :: OPT_AMPLOC_PERCENT_95 = 95

  !> Tukey variant option (default) for PROP_ADJUST_FILTER_METHOD, the outlier filtering method
  !! for adjustment estimates
  character(len=*), parameter :: OPT_FILTER_METHOD_TUKEY = "conf"
  !> Bayesian (BIC) option for PROP_ADJUST_FILTER_METHOD, the outlier filtering method for
  !! adjustment estimates
  character(len=*), parameter :: OPT_FILTER_METHOD_BIC = "bicf"
  !> "Both" Tukey and BIC option for PROP_ADJUST_FILTER_METHOD, the outlier filtering method for
  !! adjustment estimates
  character(len=*), parameter :: OPT_FILTER_METHOD_BOTH = "both"
  !> "Neither" Tukey nor BIC option for PROP_ADJUST_FILTER_METHOD, the outlier filtering method
  !! for adjustment estimates
  character(len=*), parameter :: OPT_FILTER_METHOD_NONE = "none"

  !> Median option for PROP_ADJUST_ESTIMATE_METHOD, the method used to determine the adjustment
  character(len=*), parameter :: OPT_EST_METHOD_MEDIAN = "med"
  !> Average option for PROP_ADJUST_ESTIMATE_METHOD, the method used to determine the adjustment
  character(len=*), parameter :: OPT_EST_METHOD_AVG = "avg"
  !> Inter-quartile range average option for PROP_ADJUST_ESTIMATE_METHOD, the method used to determine
  !! the adjustment
  character(len=*), parameter :: OPT_EST_METHOD_QUART_AVG = "qav"

end module PropertyParameters

!> @file
!! Contains parameters for constants for reading from properties files.

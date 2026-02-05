!> @brief TOB-specific property parameter keys.
!!
!! Defines configuration keys used exclusively by the TOB pipeline.
!! Kept separate from PropertyParameters to avoid altering NOAA-origin files.
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
module TOBPropertyParameters

  implicit none

  !> Path to per-station input data for TOB processing.
  !! Expected to point at the raw (or pre-TOB) element directory.
  character(len=*), parameter :: PROP_TOB_PATH_ELEMENT_DATA_IN  = "tob.path.station-element-data-in"
  !> Path to per-station output data for TOB processing.
  !! Output files will be written to this directory.
  character(len=*), parameter :: PROP_TOB_PATH_ELEMENT_DATA_OUT = "tob.path.station-element-data-out"
  !> First year for applying TOB adjustments.
  !! Years before this will be copied without adjustment. If
  !! tob.start-from-history is true, this value is overridden by
  !! the first history record with a resolvable observation time.
  character(len=*), parameter :: PROP_TOB_START_YEAR            = "tob.start-year"
  !> If true, start TOB adjustments from the first history record with a
  !! resolvable observation time; otherwise use tob.start-year.
  character(len=*), parameter :: PROP_TOB_START_FROM_HISTORY    = "tob.start-from-history"
  !> TOB-specific logger filename. If set, overrides pha.logger.filename.
  character(len=*), parameter :: PROP_TOB_LOG_FILENAME          = "tob.logger.filename"
  !> TOB-specific logger level. If set, overrides pha.logger.level.
  character(len=*), parameter :: PROP_TOB_LOG_LEVEL             = "tob.logger.level"
  !> TOB-specific logger stdout flag. If set, overrides pha.logger.print-to-stdout.
  character(len=*), parameter :: PROP_TOB_LOG_STDOUT            = "tob.logger.print-to-stdout"
  !> TOB-specific logger datestamp flag. If set, overrides pha.logger.append-datestamp.
  character(len=*), parameter :: PROP_TOB_LOG_DATESTAMP         = "tob.logger.append-datestamp"
  !> TOB-specific logger rollover flag. If set, overrides pha.logger.rollover-datestamp.
  character(len=*), parameter :: PROP_TOB_LOG_ROLLOVER          = "tob.logger.rollover-datestamp"

end module TOBPropertyParameters

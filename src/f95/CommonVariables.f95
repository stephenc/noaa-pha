!> @brief
!! Variables that are used so commonly throughout multiple modules
!! of the program that it is simpler to determine their value once and store
!! them globally. In some cases, calculation of a variable may be quite
!! difficult.
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
module CommonVariables

  !> Begin year for all series, set in properties file
  integer :: begin_year

  !> End year for the project series (current year)
  integer :: end_year

  !> Total number of months in the Period Of Record including the
  !! 12 months of lead-in data that are not used
  integer :: por_months

  !> Number of station-years of input data
  integer skyline_months

end module CommonVariables

!> @file
!! Contains variables for use throughout the program. Contains no functions or subroutines.

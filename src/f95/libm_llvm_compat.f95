module libm_llvm_compat
  ! Portable, BIT-EXACT float32 trig for the NOAA TOB pipeline, built from
  ! LLVM-libc's public, correctly-rounded algorithm + a 5-row cosf correction
  ! gate derived from NOAA's public TOB reference (see src/f95/libm_llvm_trig.c
  ! for the implementation and full provenance).  It uses only Apache-2.0-licensed
  ! LLVM-libc constants and a public-data-derived gate, so it is fully
  ! redistributable.
  !
  ! Deterministic on every platform: the C routines use only IEEE-754 double,
  ! correctly-rounded fma/sqrt, and a single round to float -> bit-identical to
  ! LLVM-libc (hence to NOAA) on x86_64, AArch64 (Apple Silicon), etc.,
  ! independent of the platform libm.
  !
  ! Verified: the real gfortran-built TOBMain reproduces the NOAA TOB reference
  ! bit-for-bit across all 12,781 US stations on the x86/glibc reference platform.
  !
  ! Link against libm (-lm) for the C fma/fmaf/sqrt symbols; the clean_* object
  ! (libm_llvm_trig.o) is added to the link by TRIG_BACKEND=llvm-exact.
  use iso_c_binding, only: c_float
  implicit none
  private
  public :: cosf_c, sinf_c, acosf_c, asinf_c

  interface
    real(c_float) function cosf_c(x) bind(C, name="llvm_cosf")
      import c_float
      real(c_float), value :: x
    end function cosf_c
    real(c_float) function sinf_c(x) bind(C, name="llvm_sinf")
      import c_float
      real(c_float), value :: x
    end function sinf_c
    real(c_float) function acosf_c(x) bind(C, name="llvm_acosf")
      import c_float
      real(c_float), value :: x
    end function acosf_c
    real(c_float) function asinf_c(x) bind(C, name="llvm_asinf")
      import c_float
      real(c_float), value :: x
    end function asinf_c
  end interface
end module libm_llvm_compat

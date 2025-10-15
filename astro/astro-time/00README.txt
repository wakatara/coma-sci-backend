
Routines for addressing astronomical time.

Notes on JD/MJD <---> UT conversion: It is correct (agrees with JPL)
if the JD is assumed to be JDUT.  Both UTC and JDUTC have leap seconds
inserted.

This means that JD and MJD are not a smoothly varying quantity, if
generated from UTC, and that differences in JD cannot be used to
compute true time intervals.

In general, the MJD reported in telescope headers is be derived from
the correspnding UTC.

The true (atomic time) is given by TAI, and a file of offsets
is here: ftp://maia.usno.navy.mil/ser7/tai-utc.dat

The time scales relevant to astronomy are:

TAI    -  atomic time

UTC    - atomic time leap-second adjusted to the earth's rotation, really
         a measure of the angular position of the earth.  Technically UTC
         should not be converted to MJDUT because of overlaps at the leap
         seconds.

MJDUTC - MJD based on the current UTC, using standard conversion function.
         This has leap seconds so that subtracting two MJDUTCs does not
         give a correct time differnce if leap seconds have been inserted.

TT     - Terrestrial Time, TT=TAI+32.184s, the time for computing
         ephemerides.

TBD    - Barycentric Dynamical Time, TT with quasi-periodic adjustments for
         General Relativistic effects.

MJDTAI - MJD that varies smoothly with TAI, derived from MJDUT
         using a function like MJDUTC-TO-MJDTAI

MJDTT  - MJD that varies smoothly with TT, derived from MJDUT
         using a function like MJDUTC-TO-MJDTT

The routine SLALIB:CORRECT-MJDUT-TO-MJDTT in the SLALIB package
performs the corrections from UT to MJDTT. F2C can be used to convert it
to Fortran using (f2cl:f2cl "/slalib/dir/dat.f") which is how we obtained
TAI-MINUS-UTC, MJDUTC-TO-MJDTAI, and MJDUTC-TO-MJDTT

# jd-time-utils - time utilities to work before 1900-01-01, using Julian Days

Includes replacement for encode-universal-time and decode-universal-time
and routines for parsing time into a structure including calendar date, fractional
year, and Julian day, second, and nanosecond.

Includes modified version of cl-date-time-parser

## Dependencies

* anaphora
* cl-ppcre
* local-time
* parse-float  - several versions OK; must have (parse-float:parse-float string)
* bordeaux-threads 

## Principles

Standard Lisp universal time (UT) is defined using 1900-00-00T00:00:00 as UT=0, and
dates before 1900 are not allowed.

This package uses the astronomical Julian Day as its internal basis, and extends UT
to negative dates.

Julian Day 0 is on January 1, 4713 BC, proleptic (extended) Julian calendar
and November 24, 4714 BC, in the proleptic Gregorian calendar.

Negative Julian days are allowed, so it works far into the past or
future.  Internally, time is represented as Julian Day, Julian Second
(in Julian Day), and Julian nanonsecond, and the span is limited by
the representation of seconds of time in the integers used (about a
billion years for 55 bits).

When converting calendar dates (YYYY,MM,DD,HH,MM,SS) to absolute time units
(Julian Day, Julian seconds, or UT seconds), note that by default dates
before Oct 15,1582 are treated as Julian, not Gregorian (no leap years).
This can be disabled using keyword ":GREGORIAN-TRANSITION NIL"

# Caveats

* Time zone is 0 (GMT) by default, not localtime.  
* There is no knowledge of Daylight Savings Time.  It does not exist in these routines.
* The included version of cl-date-time-parser seems to have had the signs of hardwired  timezones reversed. This is fixed (?).  We hope this is correct.

## Examples

#### alternatives for encode and decode universal time

encoding universal time:

````
  (jd-time-utils:encode-universal-time/extended
            1.1 2 3 31 12 1800 ;; SEC MIN HR DATE MONTH YEAR
            :time-zone 0)      ;; new keyword
  ==> 
      -3124213079  ;; negative seconds before 1900
      100000023    ;; nanoseconds (with single float rounding error)
````

decoding universal time:

````
(jd-time-utils:decode-universal-time/extended -3124213079 
==>
  (jd-time-utils:decode-universal-time/extended 
    -3124213079 
    :time-zone 0
    :gregorian-transition t   ;; only affects dates before Oct 15,1582
    :nanoseconds 100000000)   ;; nanonseconds keyword
 ==>
  1 2 3 31 12 1800   ;; SEC MIN HR DATE MONTH YEAR
  2                  ;; DAY-OF-WEEK (0-6)
  100000000          ;; NANOSECONDS
  0                  ;; TIME-ZONE
````

#### Time parsing into a DATE-TIME  structure


Using standard formats borrowed from CL-DATE-TIME-PARSER 
````
 (jd-time-utils:parse-date-time-string
 "5:42:00.1 July 4, 1976 GMT" :output-timezone 0)
 ==> 
      #S(jd-time-utils:date-time
	  :year 1976
	  :month 7
	  :day 4
	  :hour 5
	  :minute 42
	  :second 0
	  :fractional-second 0.1d0
	  :day-of-week 6
	  :ut 2414295720
	  :timezone 0  ;; see :OUTPUT-TIMEZONE 
	  :float-year 1976.5061133911404d0
	  ;; days, seconds, and nanoseconds since Julian January 1, 4713 BC
	  :julian-time #S(jd-time-utils:julian-time
                          :day 2442964
                          :second 20520
                          :nanosecond 100000000))     
````

Parse in a different timezone (EST); note default conversion to timezone 0

````
 (jd-time-utils:parse-date-time-string
	  "5:42:00.1 July 4, 1976 EST")
 ==>
   #S(jd-time-utils:date-time
	  :year 1976
	  :month 7
	  :day 4
	  :hour 10  ;; 5 hours later than above
	  :minute 42
	  :second 0
	  :fractional-second 0.1d0
	  :day-of-week 6
	  :ut 2414313720
	  :timezone 0 ;; timezone is still zero
	  :float-year 1976.5066826078983d0
	  :julian-time #S(jd-time-utils:julian-time
	                  :day 2442964
	                  :second 38520
	                  :nanosecond 100000000))
````

Using a stated format :MM-DD-YY, and turning off general formats

````
 (jd-time-utils:parse-date-time-string
	  "07/05/76" :date-convention :mm-dd-yy :try-standard-formats nil)
  ==>
   #S(jd-time-utils:date-time
      :year 1976
      :month 7
      :day 5
      :hour 0
      :minute 0
      :second 0
      :fractional-second 0.0d0
      :day-of-week 0
      :ut 2414361600
      :timezone 0
      :float-year 1976.5081967213114d0
      :julian-time #S(jd-time-utils:julian-time
                      :day 2442965
                      :second 0
                      :nanosecond 0))
````

Convert 1899-12-31T23:59:59 to a structure

````
 (jd-time-utils:build-date-time-struct-from-ut 
          -1 ;; one second before Lisp era
	  :timezone 10)
 ==>	  
  #S(jd-time-utils:date-time
      :year 1899
      :month 12
      :day 31
      :hour 13   
      :minute 59
      :second 59
      :fractional-second 0.0d0
      :day-of-week 6
      :ut -1 
      :timezone 10 ;; new output timezone (10 zones west of meridian)
      :float-year 1899.9999999682902d0
      :julian-time #S(jd-time-utils:julian-time
                      :day 2415020
                      :second 86399
                      :nanosecond 0))
````

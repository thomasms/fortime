# Fortime

A simple timer module for benchmarking Fortran code.

```Fortran
program example
    use fortime_m
    use kinds_m
    implicit none

    type(Timer) :: stopwatch
    
    real(kind=dp) :: elapsedseconds
    real(kind=dp) :: intervalseconds

    ! start it
    call stopwatch%start()
    
    ! do some work here
    ....
    
    ! elapsed and interval should be the same here
    elapsedseconds = stopwatch%elapsed()
    intervalseconds = stopwatch%interval()
    
    ! do some more work here

    ! elapsed and interval should not be the same here
    elapsedseconds = stopwatch%elapsed()
    intervalseconds = stopwatch%interval()
    
    ! end timer
    call stopwatch%finish()
    
end program example
```
You can use the macro DO_CHECKS to enable checking if timer has started for safety, by default checks are not enabled. It is not clear what performance hit this will cost.

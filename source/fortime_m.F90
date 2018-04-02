!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                                               !!
!!                  FORTIME                      !!
!!                                               !!
!!      Copyright (c) 2018, Thomas Stainer       !!
!!                                               !!
!!            All rights reserved.               !!
!!    Licensed under the 3-clause BSD license.   !!
!!                                               !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module fortime_m
    use fork_m
    implicit none
    private

    !> A stopwatch
    type, public :: Timer
    private
        logical       :: isstarted      = .false.
        real(kind=sp) :: starttime      = 0.0_sp
        real(kind=sp) :: lastelapsed    = 0.0_sp
        real(kind=sp) :: currentelapsed = 0.0_sp
    contains
        procedure :: start
        procedure :: finish
        procedure :: reset
        procedure :: elapsed
        procedure :: interval
        procedure, private :: checkstarted
    end type Timer

contains

    !> Start the timer
    !! Should only call initially or after finish is called
    !! Stops otherwise
    subroutine start(this)
        class(Timer), intent(inout) :: this

#ifdef DO_CHECKS
        if(this%isstarted .eqv. .true.)then
            stop "Timer is already started"
        endif
#endif
        this%isstarted = .true.
        call cpu_time(this%starttime)
        this%lastelapsed    = this%starttime
        this%currentelapsed = this%starttime

    end subroutine start

    !> End the timer
    !! Should only be called after start is called
    !! Stops otherwise
    subroutine finish(this)
        class(Timer), intent(inout) :: this

#ifdef DO_CHECKS
        call this%checkstarted()
#endif
        call cpu_time(this%lastelapsed)
        this%currentelapsed = this%lastelapsed
        this%isstarted = .false.

    end subroutine finish

    !> Reset the timer
    !! Does not stop or restart the timer
    !! Just resets the times to the current
    !! Should only be called after start is called
    !! Stops otherwise
    subroutine reset(this)
        class(Timer), intent(inout) :: this

#ifdef DO_CHECKS
        call this%checkstarted()
#endif
        call cpu_time(this%starttime)
        this%lastelapsed    = this%starttime
        this%currentelapsed = this%starttime
    end subroutine reset

    !> Get the elapsed time now
    subroutine elapsed(this,time)
        class(Timer), intent(inout) :: this
        real(kind=sp), intent(out) :: time

        real(kind=sp) :: endtime

#ifdef DO_CHECKS
        call this%checkstarted()
#endif
        call cpu_time(endtime)
        time = endtime - this%starttime
        this%lastelapsed = this%currentelapsed
        this%currentelapsed = endtime

    end subroutine elapsed

    !> Get the interval time between last elapsed time and now
    subroutine interval(this, time)
        class(Timer), intent(in)   :: this
        real(kind=sp), intent(out) :: time

#ifdef DO_CHECKS
        call this%checkstarted()
#endif
        time = this%currentelapsed - this%lastelapsed

    end subroutine interval

    !! Checks if timer start is called
    !! Stops otherwise
    subroutine checkstarted(this)
        class(Timer), intent(in) :: this

        if(this%isstarted .eqv. .false.) then
            stop "Timer not started"
        endif
    end subroutine checkstarted

end module fortime_m

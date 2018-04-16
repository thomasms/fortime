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

module timefunctor_m
    use fork_m
    use fortime_m
    implicit none
    private

    !> Function to get the time taken for a function
    !! Returns the time in seconds (s)
    public :: timeit

    !> Extend this to benchmark functor
    type, public, abstract :: TimeFunctor
    private
        real(kind=sp)             :: time = 0.0_sp     !< Time Taken (secs)
                                                        !! we don't want people to set this
        integer(kind=ki2), public :: repeats = 3_ki2    !< Best of three by default
        integer(kind=ki4), public :: iterations = 1_ki4 !< By default we do one iteration
    contains
        procedure :: gettime
        procedure :: printsummary
        procedure(functor_timeit), deferred :: run
    end type TimeFunctor

    abstract interface
        subroutine functor_timeit(this)
            import :: TimeFunctor
            implicit none
            class(TimeFunctor), intent(inout) :: this
        end subroutine functor_timeit
    end interface

contains

    !> get the time taken for the last run
    subroutine gettime(this, time)
        class(TimeFunctor), intent(in) :: this
        real(kind=sp), intent(out)    :: time

        time = this%time
    end subroutine gettime

    !> Print the time taken
    subroutine printsummary(this)
        class(TimeFunctor), intent(in) :: this

        write(*,"(A, ES14.7, A, I10.1, A, I3.1, A)") "Time taken:", this%time, &
                 & " s, for:", this%iterations, " iterations, using: ", this%repeats, &
                 & " repeats"

    end subroutine printsummary

    !> Get the CPU time taken for functor
    subroutine timeit(func)
        class(TimeFunctor), intent(inout) :: func

        real(kind=sp)     :: time, average_time
        integer(kind=ki2) :: r
        integer(kind=ki4) :: i

        type(Timer) :: stopwatch

        average_time = 0.0_sp
        do r=1,func%repeats
            call stopwatch%start()
            do i=1, func%iterations
                call func%run()
            end do
            call stopwatch%elapsed(time)
            average_time = average_time + time
            call stopwatch%stop()
        enddo

        average_time = average_time/func%repeats
        func%time = average_time
    end subroutine timeit

end module timefunctor_m

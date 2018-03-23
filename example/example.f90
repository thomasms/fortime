program fortimexample
    use kinds_m
    use fortime_m
    implicit none

    type(Timer) :: stopwatch

    ! start
    print *, "Starting stopwatch..."
    call stopwatch%start()
    print *, stopwatch%elapsed(), " seconds"

    ! do some silly work
    call runloop(10)
    call runloop(100)
    call runloop(10)
    call runloop(1000)
    call runloop(10000)
    call runloop(100)
    call runloop(100)
    call runloop(100)
    call runloop(100)
    call runloop(100)
    call runloop(100)
    call runloop(100)
    call runloop(20000)

    ! end
    call stopwatch%finish()

contains

    subroutine runloop(iterations)
        integer(kind=sp), intent(in) :: iterations

        real(kind=dp), dimension(:,:), allocatable :: mat

        print *, "Doing matrix work for size:", iterations

        allocate(mat(iterations,iterations))
        mat = dowork(iterations)
        deallocate(mat)

        print *, "ELAPSED:", stopwatch%elapsed(), " seconds"
        print *, "INTERVAL:", stopwatch%interval(), " seconds"

    end subroutine runloop

    function dowork(iterations) result(matrix)
        integer(kind=sp), intent(in) :: iterations
        real(kind=dp), dimension(iterations, iterations) :: matrix

        integer(kind=sp) :: i,j

        !> do silly work
        do i=1, iterations
            do j=1, iterations
                matrix(i,j) = i*j/3.14
            enddo
        enddo

    end function dowork

end program fortimexample

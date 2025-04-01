subroutine printgrid(vector)
    integer, dimension(20, 20), intent(in) :: vector

    integer :: i, j
    do i = 1, 20
        do j = 1, 20
            if (vector(i, j) == 1) then
                write(*, '(A)', advance='no') 'O'
            else
                write(*, '(A)', advance='no') '.'
            end if
        end do
        write(*, *)
    end do

end subroutine printgrid

subroutine advancegeneration(grid, new_grid)
    integer, dimension(20, 20), intent(in) :: grid
    integer, dimension(20, 20), intent(out) :: new_grid

    integer :: i, j, count

    integer :: up, down, left, right

    new_grid = 0

    ! Loop through each cell in the grid
    do i = 1, 20
        do j = 1, 20
            count = 0

            up = i - 1
            down = i + 1
            left = j - 1
            right = j + 1

            if (up >= 1) then
                count = count + grid(up, j)
            end if

            if (down <= 20) then
                count = count + grid(down, j)
            end if

            if (left >= 1) then
                count = count + grid(i, left)
            end if

            if (right <= 20) then
                count = count + grid(i, right)
            end if
            
            if (up >= 1 .and. left >= 1) then
                count = count + grid(up, left)
            end if

            if (up >= 1 .and. right <= 20) then
                count = count + grid(up, right)
            end if

            if (down <= 20 .and. left >= 1) then
                count = count + grid(down, left)
            end if

            if (down <= 20 .and. right <= 20) then
                count = count + grid(down, right)
            end if

            ! Apply the rules of Conway's Game of Life
            if (grid(i, j) == 1) then
                ! if it has less than 2 neighbors, it dies
                if (count < 2) then
                    new_grid(i, j) = 0
                else if (count == 2 .or. count == 3) then
                    new_grid(i, j) = 1
                else if (count > 3) then
                    new_grid(i, j) = 0
                end if
            end if

            if (grid(i, j) == 0) then
                ! if it has exactly 3 neighbors, it becomes alive
                if (count == 3) then
                    new_grid(i, j) = 1
                end if
            end if
        end do
    end do

end subroutine advancegeneration

program gameoflife
    implicit none
    integer, dimension(20, 20) :: grid
    integer, dimension(20, 20) :: new_grid

    integer :: generations

    grid = 0

    ! Initialize the grid with a glider pattern
    grid(5, 5) = 1
    grid(6, 5) = 1
    grid(7, 5) = 1
    grid(7, 4) = 1
    grid(6, 3) = 1        

    do generations = 1, 40
        call printgrid(grid)
        write(*, *) 'Generation:', generations
        call sleep(1)  ! Pause for 1 second to visualize the generations

        call advancegeneration(grid, new_grid)
        grid = new_grid
    end do

end program gameoflife

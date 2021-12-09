! Advent of Code 2021 : https://adventofcode.com/2021
! Puzzle for Day 05 part 01 : 
! Build with:  gfortran -static-libgcc -o aoc_day05_p1 aoc_day05_p1.f90

program aoc_day05_p1

	use, intrinsic :: iso_fortran_env
	implicit none

	! Declare and set constants the AOC day and part
	integer,parameter :: day = 5, part = 1
	
	! Declare file related variables
	character(len=*), parameter :: filename = "day05-input.txt"
	character(len=*), parameter :: error1 = "file: '"//''//filename//''//"' not found"

	! Variables to track and manage the program
	integer :: total_lines = 0  ! total number of vent lines in input file
	integer :: i,j,k,m  ! do loop variables
	
	! Arrays to manage the vent lines and to track intersections
	integer, allocatable :: begin_xy(:,:), end_xy(:,:)
	integer, allocatable :: p1_intersect(:,:)
	
	! Output startup message
	call display_welcome(day,part)

	if (.NOT. file_exists(filename)) stop error1
	total_lines = get_line_count(filename)
	write(*,'(A,I0)') " » Total line vent entries: ",total_lines
	
	call get_vent_lines(filename, total_lines, begin_xy, end_xy)

	j=max(maxval(begin_xy(1,:)),maxval(end_xy(1,:)))
	k=max(maxval(begin_xy(2,:)),maxval(end_xy(2,:)))
	
	allocate(p1_intersect(j,k))
	
	p1_intersect = 0

	do i=1,total_lines
		if (begin_xy(1,i) .eq. end_xy(1,i)) then
			j=min(begin_xy(2,i),end_xy(2,i))
			k=max(begin_xy(2,i),end_xy(2,i))
			p1_intersect(begin_xy(1,i),j:k) = p1_intersect(begin_xy(1,i),j:k) + 1
		else if (begin_xy(2,i) .eq. end_xy(2,i)) then
			j=min(begin_xy(1,i),end_xy(1,i))
			k=max(begin_xy(1,i),end_xy(1,i))
			p1_intersect(j:k,begin_xy(2,i)) = p1_intersect(j:k,begin_xy(2,i)) + 1
		else
			j = 1
			k = 1
			if (begin_xy(1,i) .gt. end_xy(1,i)) j = -1
			if (begin_xy(2,i) .gt. end_xy(2,i)) k = -1
		end if
	end do
	
	write (*,'(A,I0)') " » Horizontal and vertical lines where > two vent lines cross (Part 1): ", count(p1_intersect .gt. 1)

	print *, ""
	
	deallocate(begin_xy)
	deallocate(end_xy)
	deallocate(p1_intersect)
	
contains

	! Display the AoC puzzle day and part being resolved  
	subroutine display_welcome(day,part)
		implicit none
		integer, intent(in) :: day,part
		print *,""
		write (*,'(A,I2.2,A,I2.2)') "AoC 2021 - solution for Day ",day," part ",part
		print *,""
	end subroutine display_welcome

	! Check input filename exists 
	function file_exists(filename) result (exists)
		implicit none
		character(len=*), intent(in) :: filename
		logical :: exists ! variable for the return.  NB: no intent(out) needed.
				
		inquire(file=filename, exist=exists)
	end function file_exists

	! Open the file and count the number of line vents it contains
    function get_line_count(filename) result (lines)
        implicit none
        character(len=*), intent(in) :: filename
        integer :: fileunit,io
        integer :: lines
        
        open(newunit=fileunit, file=filename, action='read', position='rewind')
        lines = 0
        do  
            read(fileunit,*,iostat=io)
            if ( io /= 0 ) exit
            lines = lines + 1
        end do
        close(fileunit)
    end function get_line_count

    ! Read in the vent lines X1,Y1 and X2,Y2 coordinates from puzzle data source file
    subroutine get_vent_lines(filename, total_lines, begin_xy, end_xy)
        character(len=*), intent(in) :: filename
        integer, intent(in) :: total_lines
        integer, allocatable, intent(inout) :: begin_xy(:,:), end_xy(:,:)
        integer :: fileunit, io, i
        character(len=2) :: A   ! account for '->' separator in in file
        
        open(newunit=fileunit, file=filename, action='read', position='rewind')     
        
        allocate(begin_xy(2,total_lines))
        allocate(end_xy(2,total_lines))
        
        do i=1,total_lines
            read(fileunit,*) begin_xy(:,i),A,end_xy(:,i)        
        end do
        
        close(fileunit)
    end subroutine get_vent_lines

end program aoc_day05_p1

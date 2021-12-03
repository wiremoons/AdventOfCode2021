! Advent of Code 2021 : https://adventofcode.com/2021
! Puzzle for Day 01 part 02 : 
! Build with:  gfortran -static-libgcc -o aoc_day01_p2 aoc_day01_p2.f90

program aoc_day01_p2

	use, intrinsic :: iso_fortran_env
	implicit none

	! Declare and set constants the AOC day and part
	integer,parameter :: day = 1, part = 2

	! Declare file related variables
	integer :: rows = 0, total_incr=0
	character(len=*), parameter :: filename = "day01-input.txt"
	character(len=*), parameter :: error1 = "file: 'day01-input.txt' not found"

	! Output startup message
	call display_welcome(day,part)

	if (.NOT. file_exists(filename)) stop error1
	rows = get_row_count(filename)
	total_incr = get_depth_sliding_increase_count(filename,rows)
	
	! Display the number rows in the input file
	write(*,'(A,I4.4)') " » Number of sonar readings: ",rows
	write(*,'(A,I4.4)') " » Total depth increases in sonar readings: ",total_incr
	
	
contains

	! Display the AoC puzzle day and part being resolved  
	subroutine display_welcome(day,part)
		implicit none
		integer, intent(in) :: day,part
		write (*,'(A,I2.2,A,I2.2)') "AoC 2021 - solution for Day ",day," part ",part
	end subroutine display_welcome


	! Check input filename exists 
	function file_exists(filename) result (exists)
		implicit none
		character(len=*), intent(in) :: filename
		logical :: exists ! type for the return.  NB: no intent(out) needed.
				
		inquire(file=filename, exist=exists)
	end function file_exists
	

	! Open the file and count the number of rows it contains
	function get_row_count(filename) result (rows)
		implicit none
		character(len=*), intent(in) :: filename
		integer :: fileunit,io
		integer :: rows ! type for the return.  NB: no intent(out) needed.
		
		open(newunit=fileunit, file=filename, action='read', position='rewind')
		rows = 0
		do  
			read(fileunit,*,iostat=io)
			if ( io /= 0 ) exit
			rows = rows + 1
		end do
		close(fileunit)
	end function get_row_count
	
	
	! Solve the problem : total number of three sliding depth increases for each sonar reading 
	function get_depth_sliding_increase_count(filename,rows) result (total_incr)
		implicit none
		character(len=*), intent(in) :: filename
		integer :: fileunit,io,rows
		integer,allocatable :: input(:)
		integer :: total_incr ! type for the return.  NB: no intent(out) needed.
		
		open(newunit=fileunit, file=filename, action='read', position='rewind')
		allocate(input(rows))
		read(fileunit,*) input
		close(fileunit)
		total_incr = count(input(1:rows-3).LT.input(4:rows))
		deallocate(input)
	end function get_depth_sliding_increase_count

end program aoc_day01_p2
! Advent of Code 2021 : https://adventofcode.com/2021
! Puzzle for Day 02 part 02 : 
! Build with:  gfortran -static-libgcc -o aoc_day02_p2 aoc_day02_p2.f90

program aoc_day02_p2

	use, intrinsic :: iso_fortran_env
	implicit none

	! Declare and set constants the AOC day and part
	integer,parameter :: day = 2, part = 2
	
	! AOC answer
	integer :: total_pos = 0

	! Declare file related variables
	character(len=*), parameter :: filename = "day02-input.txt"
	character(len=*), parameter :: error1 = "file: '"//''//filename//''//"' not found"

	! Output startup message
	call display_welcome(day,part)

	if (.NOT. file_exists(filename)) stop error1

	total_pos = get_movement_totals(filename)
	
	! Display the solution summary
	write(*,'(A,I10.10)') " » ANSWER: Final horizontal position multiplied by final depth: ",total_pos
	
	
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
	
	
	! Solve the problem : total number of depth increases for each sonar reading 
	function get_movement_totals(filename) result (total_pos)
		implicit none
		character(len=*), intent(in) :: filename
		integer :: fileunit,io,io_err,X,total_for=0,total_up=0,total_down=0,final_depth=0,aim=0,depth=0
		character(len=10) :: data_in
		integer :: total_pos ! type for the returned answer
		
		open(newunit=fileunit, file=filename, action='read', position='rewind')
		
		do
			read(fileunit,*,iostat=io_err) data_in,X
			if(io_err .NE. 0) exit
			
			select case (trim(data_in))
			case ("forward")
				total_for = total_for + X
				depth = depth + (aim * X)
			case ("up")
				total_up = total_up + X
				aim = aim - X
			case ("down")
				total_down = total_down + X
				aim = aim + X
			end select
		end do
		
		close(fileunit)

		final_depth = depth
		
		write (*,'(A)') "Calculating submarine movement:"
		write (*,'(A,I4.4)') " » Total forward: ",total_for
		write (*,'(A,I4.4)') " » Total up: ",total_up
		write (*,'(A,I4.4)') " » Total down: ",total_down
		write (*,'(A,I4.4)') " » Total aim: ",aim
		write (*,'(A,I8.8)') " » Final depth: ",final_depth
		
		total_pos = final_depth * total_for

	end function get_movement_totals

end program aoc_day02_p2

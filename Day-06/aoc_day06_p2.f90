! Advent of Code 2021 : https://adventofcode.com/2021
! Puzzle for Day 06 part 02 : 
! Build with:  gfortran -static-libgcc -o aoc_day06_p2 aoc_day06_p2.f90

! Run time on Apple mac Mini M1 (arm64) :  .007 of a seconds
! ./aoc_day06_p2  0.00s user 0.00s system 62% cpu 0.007 total

! Thanks to Steve Lionel (sblionel) for sharing his 'part 2' solution on the Fortran
! Language Discourse. This helped me to understand how to 'fix' my rather poor part 1 
! effort - which would probably still be running had I stuck with that approach :)
! See post: https://fortran-lang.discourse.group/t/advent-of-code-2021/2346/17

program aoc_day06_p1

	use, intrinsic :: iso_fortran_env
	implicit none

	! Declare and set constants the AOC day and part
	integer,parameter :: day = 6, part = 2
	
	! Declare file related variables
	character(len=*), parameter :: filename = "day06-input.txt"
	character(len=*), parameter :: error1 = "file: '"//''//filename//''//"' not found"

	! Variables to track and manage the program
	integer (8) :: total_fish = 0
	integer,parameter :: cycles = 256  ! daily cycles (18 = test / 80 = part 1/ 256 = part2)
	integer :: c,j,s
	integer (8) :: fishArray(0:8)
	integer (8) :: fishTracker(0:8)
	
	! Output startup message
	call display_welcome(day,part)

	if (.NOT. file_exists(filename)) stop error1
	
	call get_fishArray(filename, fishArray)
	fishTracker = 0
	
	total_fish = sum(fishArray)

	write(*,'(A,I0)') " » Total Lantern fish at start: ",total_fish
	write(*,'(A,I0)') " » Number of daily cycles to model: ",cycles
	
	! now calculate the number of fish over the 'cycles' period	
	do c=1,cycles
		! copy the current fish status to the tracker - excluding 'day 0' which is respawn 
		fishTracker(0:7) = fishArray(1:8)
		! copy and add the respawn fish (as exclude above) to 'day 6' position in the tracker
		fishTracker(6) = fishTracker(6) + fishArray(0)
		! copy the same respawn fish amount to 'day 8' of the tracker -> new born fish!
		fishtracker(8) = fishArray(0)
		! upadte the total fish count with any in 'day 8' as they are new borns
		total_fish = total_fish + fishTracker(8)
		! DONE : now reset the 'fishArray' to the newly calculated state ready for next loop
		fishArray = fishTracker
		! Uncomment below to look at per day calculations
		!print *, "Cycles completed: ",c," total fish count: ",total_fish
	end do
	
	print *, ""
	write(*,'(A,I0)') " » Total Lantern fish created: ",total_fish
	print *, ""

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

    ! Read in the list of Lantern fish from puzzle data source file
    subroutine get_fishArray(filename, fishArray)
        character(len=*), intent(in) :: filename
        integer (8), intent(inout) :: fishArray(0:8)
        integer :: fileunit, io, I
        character(len=1) :: A   ! account for ',' separator in in file
 
 		! ensure array has known state for initial values
 		fishArray = 0
 
        open(newunit=fileunit, file=filename, action='read', position='rewind')
        
        do
        	read(fileunit,'(I1,A1)',advance="no",iostat=io) I,A
        	if (io /= 0) exit
        	fishArray(I) = fishArray(I)+1  ! add fish by array index to existing
        end do
        fishArray(I) = fishArray(I)+1
        
        close(fileunit)
        
    end subroutine get_fishArray

end program aoc_day06_p1
! Advent of Code 2021 : https://adventofcode.com/2021
! Puzzle for Day 21 part 02 :
! Build (or run) with:
! gfortran -static-libgcc -o aoc_day21_p2 aoc_day21_p2.f90
! fpm run aoc_day21_p2

program aoc_day21_p2

    use, intrinsic :: iso_fortran_env, stderr => error_unit
    implicit none

    ! Declare and set constants the AOC day and part
    integer, parameter :: day = 21, part = 2
    
    ! Variables to track and manage the program
    integer :: turns=1,rollscore=0,roll_count=0
    integer(8) :: totalscore=0,part1_answer=0
    
    type Player
    	integer :: score = 0
    	integer :: roll_total = 0
    	integer :: move = 0
    	integer :: position = 0
    	logical :: next = .false.
    end type
    type(Player) :: p1,p2

    ! Puzzle input variables
    ! - Prod uses: day21-input.txt
    !    Player 1 starting position: 10
    !    Player 2 starting position: 4
    ! - Test uses: day21-TEST-input.txt
    !    Player 1 starting position: 4
    !    Player 2 starting position: 8
    ! set required starting position for each player below:
! 	p1%position = 10
! 	p2%position = 4
	! TEST VALUES BELOW:
	p1%position= 4
	p2%position = 8

    ! Output startup message
    call display_welcome(day, part)
    write (*, '(A,/)') "Game starting with run data:"
    write (*, '(A,I0)') " - Player 1 starting positon: ",p1%position
    write (*, '(A,I0,/)') " - Player 2 starting positon: ",p2%position
    
    ! start with Player 1
    p1%next = .true.
    
    ! run for TEST positions
    do while (p1%score < 21 .and. p2%score < 21)
        rollscore = next_roll_u1() + next_roll_u1() + next_roll_u1()
        totalscore = totalscore + rollscore
        roll_count = roll_count + 3
            	        
        if (p1%next) then
        	p1%roll_total = p1%roll_total + 1
        	p1%move = next_position(p1%position,rollscore)
        	p1%position = p1%move
			p1%score = p1%score + p1%move
			p1%next = .false.
			p2%next = .true.
        else
        	p2%roll_total = p2%roll_total + 1
        	p2%move = next_position(p2%position,rollscore)
        	p2%position = p2%move
			p2%score = p2%score + p2%move
			p1%next = .true.
			p2%next = .false.
        end if
        
        ! uncomment for updates on each loop
        !write (*,'(A,I0)') " > Total rolls performed: ",roll_count
        !write (*,'(A,I0,A,I0)') " > Player 1 position: ",p1%position," and Player 2 position: ",p2%position
        !write (*,'(A,I0,A,I0)') " > Player 1 turns: ",p1%roll_total," and Player 2 position: ",p2%roll_total
		!write (*,'(A,I0,A,I0,/)') " > Player 1 score: ",p1%score," and Player 2 score: ",p2%score
        turns = turns + 1
    end do

	! display a summary    
	write (*,'(A,I0)') "Total rolls performed: ",roll_count
	write (*,'(A,I0,A,I0)') " > Player 1 position: ",p1%position," and Player 2 position: ",p2%position
	write (*,'(A,I0,A,I0)') " > Player 1 turns: ",p1%roll_total," and Player 2 position: ",p2%roll_total
	write (*,'(A,I0,A,I0,/)') " > Player 1 score: ",p1%score," and Player 2 score: ",p2%score
    
    ! winner exists as loop ended - which player lost?
    if (p2%next) then
    	write (*,'(A)') "Player 2 lost."
    	part1_answer = roll_count * p2%score
    else
    	write (*,'(A)') "Player 1 lost."
    	part1_answer = roll_count * p1%score
    end if

	write (*,'(A,I0,/)') " » Part One answer : ",part1_answer

contains

    ! Display the AoC puzzle day and part being resolved
    subroutine display_welcome(day, part)
        implicit none
        integer, intent(in) :: day, part
        print *, ""
        write (*, '(A,I2.2,A,I2.2)') "AoC 2021 - solution for Day ", day, " part ", part
        print *, ""
    end subroutine display_welcome


    ! Provide the next dice roll value where values are in sequence between 1 - 100
    function next_roll_u1()
        implicit none
        integer :: next_roll_u1
        integer, save :: dice_status=1

		! Check the dice has not reach the 100 limit on last roll
		if (dice_status > 100) then
			dice_status = 1
		end if
		
		next_roll_u1 = dice_status		
		! increment the dice ready for next request
		dice_status = dice_status + 1
    end function next_roll_u1
    
        ! Provide the next dice roll value where values are in sequence between 1 - 100
    function next_roll_u2()
        implicit none
        integer :: next_roll_u2
        integer, save :: dice_status=2

		! Check the dice has not reach the 100 limit on last roll
		if (dice_status > 100) then
			dice_status = 2
		end if
		
		next_roll_u2 = dice_status		
		! increment the dice ready for next request
		dice_status = dice_status + 2
    end function next_roll_u2
    
        ! Provide the next dice roll value where values are in sequence between 1 - 100
    function next_roll_u3()
        implicit none
        integer :: next_roll_u3
        integer, save :: dice_status=3

		! Check the dice has not reach the 100 limit on last roll
		if (dice_status > 100) then
			dice_status = 3
		end if
		
		next_roll_u3 = dice_status		
		! increment the dice ready for next request
		dice_status = dice_status + 3
    end function next_roll_u3
    
    
    ! calculate the next position based on the last position and current dice role score
    function next_position(position,rollscore)
        implicit none
        integer, intent(in) :: position,rollscore
        integer :: next_position,t
        
        t = position + rollscore
		next_position = MOD(t,10)
		
		if (next_position == 0) then
			next_position = 10
		end if
    end function next_position


end program aoc_day21_p2
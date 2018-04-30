program f35
  use euler
  implicit none
  integer :: n, sum
  logical :: b

  sum = 0

  do n=1,10**6
    if (ispalindromic(n).and.ispalindromic2(n)) then
      sum = sum + n
    end if
  end do

  print*, sum

end program

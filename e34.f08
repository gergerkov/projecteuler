program e34
implicit none

  logical :: isprime
  integer :: countdig
  print *, isprime(17), isprime(18)
  print *, countdig(1234)

end program

logical function isprime(n)
  integer :: n, i
  isprime = .true.
  if (n .lt. 3) return
  do i=2,int(sqrt(real(n)))
    if (modulo(n, i) == 0) then
      isprime = .false.
      return
    end if
  end do
end function

function perms(n)
  integer, intent(in) :: n, countdig
  integer, dimension(countdig(n)) :: perms
end function


pure integer function countdig(n)
  integer n
  countdig = ceiling(log10(real(n + 1)))
end function

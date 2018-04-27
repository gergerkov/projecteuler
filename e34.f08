program e34
implicit none

  logical :: isprime, allpermsprime, b
  integer :: countdig, ii, count
  real :: start, finish
  call cpu_time(start)

  count = 0
  do ii = 2, 10**6
    if (allpermsprime(ii)) then
      print*, ii
      count = count + 1
    end if
  end do

  print*, count

  call cpu_time(finish)
  print '("Time = ",f6.3," seconds.")',finish-start

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

logical function allpermsprime(n)
  integer :: i, n, m, dignum
  logical :: isprime
  m = n
  allpermsprime = .true.
  dignum = ceiling(log10(real(n + 1)))
  do i = 1, dignum
    m = 10**(dignum - 1) * modulo(m, 10) + m / 10
    if (.not.isprime(m)) then
      allpermsprime = .false.
      return
    end if
  end do
end function

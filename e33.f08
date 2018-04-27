program e33
  implicit none

  integer :: n, fact, summa = 0
  logical :: issumfact

  do n=3, 2999999
    if (issumfact(n)) summa = summa + n
  end do

print *, summa

end program

integer function fact(n)
  integer :: i, n
  fact = 1
  do i = 1, n
     fact = fact * i
  end do
end

logical function issumfact(orig)
  integer :: sum, orig, n, fact
  n = orig
  sum = 0
  do while (n > 0)
    sum = sum + fact(modulo(n,10))
    n = n / 10
  end do
  issumfact = sum == orig
end

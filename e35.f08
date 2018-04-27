program f35
implicit none

  integer :: n
  logical :: palindromic
  do n=1,1000
    if (palindromic(n)) print*, n
  end do
end program

logical function palindromic(n)
  integer l, m, n, t
  logical :: b
  m = n
  b = .false.
  10 continue
    call pieces(l,m,t)
    palindromic = l.eq.t
  if (m > 9 .and. l.eq.t) goto 10
end

subroutine pieces(l, m, t)
  integer, intent(inout) :: m
  integer, intent(out) :: l, t
  integer :: mag, n
  n = m
  t = modulo(n, 10)
  m = n / 10
  mag = 10 ** (ceiling(log10(real(n + 1))) - 1)
  l = n / mag
  m = m - l*mag/10
end

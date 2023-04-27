  module eam_wrap
    use iso_c_binding, only: c_double, c_int
      implicit none
  contains
    subroutine c_force_eam (natms, ndim, box, R, F, U) bind(c)
      integer(c_int), intent(in) :: natms(2)
      integer(c_int), value, intent(in) :: ndim
      real(c_double), intent(in) :: box(3)
      real(c_double), intent(inout) :: U(1), R(ndim), F(ndim)
      call force_eam(natms, ndim, box, R, F, U)
    end subroutine c_force_eam
  end module eam_wrap

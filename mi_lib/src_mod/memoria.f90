module memoria

    use penf

    implicit none

    private
    public :: reservarCampo, reservarConnect, reservarPuntos,libCampo, libConnect, libEsc, libPuntos

contains

subroutine reservarConnect(offset, cell_type, num_cells)
        
    integer, intent(in)                         ::   num_cells
    integer(I4P), allocatable, intent(inout)    ::   offset(:)
    integer(I1P), allocatable, intent(inout)    ::   cell_type(:) 

    if(.not.allocated(cell_type)) allocate(cell_type(1:num_cells))
    if(.not.allocated(offset)) allocate(offset(1:num_cells))

end subroutine reservarConnect

subroutine libConnect(connect, offset, cell_type)
        
    integer(I4P), allocatable, intent(inout)    ::  connect(:), offset(:)
    integer(I1P), allocatable, intent(inout)    ::  cell_type(:) 

    if(allocated(connect)) deallocate(connect)
    if(allocated(offset)) deallocate(offset)
    if(allocated(cell_type)) deallocate(cell_type)

end subroutine libConnect

subroutine reservarPuntos(x_points, y_points, z_points, num_points)

    real(R4P), allocatable, intent(inout)       ::  x_points(:), y_points(:) ,z_points(:)  
    integer(I4P), intent(in)                    ::  num_points 

    if(.not.allocated(x_points)) allocate(x_points(1:num_points))
    if(.not.allocated(y_points)) allocate(y_points(1:num_points))
    if(.not.allocated(z_points)) allocate(z_points(1:num_points))
    
end subroutine reservarPuntos   

subroutine libPuntos(x_points, y_points, z_points)

    real(R4P), allocatable, intent(inout)       ::  x_points(:), y_points(:) ,z_points(:)   

    if(allocated(x_points)) deallocate(x_points)
    if(allocated(y_points)) deallocate(y_points)
    if(allocated(z_points)) deallocate(z_points)

end subroutine libPuntos

subroutine reservarCampo(camp_x, camp_y, camp_z, num_points)

    real(R8P), allocatable, intent(inout)       ::  camp_x(:), camp_y(:) ,camp_z(:)   
    integer(I4P), intent(in)                    ::  num_points

    if(.not.allocated(camp_x)) allocate(camp_x(1:num_points))
    if(.not.allocated(camp_y)) allocate(camp_y(1:num_points))
    if(.not.allocated(camp_z)) allocate(camp_z(1:num_points))

end subroutine reservarCampo

subroutine libCampo(camp_x, camp_y, camp_z)

    real(R8P), allocatable, intent(inout)       ::  camp_x(:), camp_y(:) ,camp_z(:)   

    if(allocated(camp_x)) deallocate(camp_x)
    if(allocated(camp_y)) deallocate(camp_y)
    if(allocated(camp_z)) deallocate(camp_z)

end subroutine libCampo

subroutine libEsc(esc)
    
    real(R8P), allocatable, intent(inout)       ::  esc(:)

    if(allocated(esc)) deallocate(esc)

end subroutine libEsc

end module memoria
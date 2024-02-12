program plantilla

    !--------------------------------------------------------------------
    ! Modulos
    !--------------------------------------------------------------------
    
    ! Modulos de la libreria VTK-Fortran
    use penf
    use vtk_fortran, only : vtk_file, pvtk_file

    ! Modulos propios
    use memoria
    use file_management

    implicit none

    !--------------------------------------------------------------------
    ! VARIABLES
    !--------------------------------------------------------------------
    
    ! Definimos las variables............................................
    type(vtk_file)                      ::  a_vtk_file    ! Elemento con las funciones para crear VTKFiles
    type(pvtk_file)                     ::  a_pvtk_file   ! Elemento con las funciones para crear PVTKFiles
    type(Unstructured_data)             ::  data    
    integer(I4P)                        ::  error         ! Variable para guardar los errores de las funciones
    character(len = 40), dimension(2)   ::  filename_vtu  ! Nombre del archivo que vamos a crear
    
    !--------------------------------------------------------------------
    ! CODIGO
    !--------------------------------------------------------------------
    
    ! Variables comunes
    data%a_vtk_file = a_vtk_file
    data%a_pvtk_file = a_pvtk_file

    ! PRIMER ARCHIVO .vtu................................................
    
    data%num_cells   =  0_I4P
    data%num_points  =  0_I4P

    ! Reservamos memoria para las variable allocatable.
    call reservarPuntos(data%x_points, data%y_points, data%z_points, data%num_points)
    call reservarConnect(data%offset, data%cell_type, data%num_cells)
    call reservarCampo(data%camp_x, data%camp_y,data%camp_z, data%num_points)
    allocate(data%esc(1:data%num_points))

    ! Inicializacion de las variables 
    data%mesh_topology = ''   
    data%format = ''                                                  
    data%filename_vtu = ''
    filename_vtu(1) =data%filename_vtu;


    data%x_points = []
    data%y_points = []
    data%z_points = []  

    data%cell_type =[]
    data%offset = [] 
    
    allocate(data%connect(1:data%offset(data%num_cells)))
    data%connect = [] 

    data%esc =[]

    data%camp_x =[] 
    data%camp_y =[] 
    data%camp_z =[] 


    ! Creamos el archivo
    call check_error(data%open_vtu())
    call check_error(data%a_vtk_file%xml_writer%write_dataarray(data_name = 'Temperature', x=data%esc))
    call check_error(data%a_vtk_file%xml_writer%write_dataarray(data_name = 'Camp electrico real',&
        x=data%camp_x, y=data%camp_y, z=data%camp_z))
    
    call check_error(data%close_vtu())
 
    !Liberamos memoria para las variables que cambian en el segundo archivo
    call libConnect(data%connect, data%offset, data%cell_type)

    !--------------------------------------------------------------------
    ! FUNCIONES
    !--------------------------------------------------------------------

    contains

    subroutine check_error(error)
        integer, intent(in) :: error
        if ( error /= 0 ) then
            print *, "ERROR ", error
        end if        
    end subroutine check_error

end program plantilla
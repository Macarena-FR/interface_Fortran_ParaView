program ejemplo_prisma_hex

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
    
    data%num_cells   =   6_I4P
    data%num_points  =   14_I4P

    ! Reservamos memoria para las variable allocatable.
    call reservarPuntos(data%x_points, data%y_points, data%z_points, data%num_points)
    call reservarConnect(data%offset, data%cell_type, data%num_cells)
    call reservarCampo(data%camp_x, data%camp_y,data%camp_z, data%num_points)
    allocate(data%esc(1:data%num_points))

    ! Inicializacion de las variables 
    data%mesh_topology = 'UnstructuredGrid'   
    data%format = 'ascii'                                                  
    data%filename_vtu = 'XML_vtu_hex_3d.vtu'
    filename_vtu(1) =data%filename_vtu;


    data%x_points = [0.0, 1.0, 0.5, -0.5, -1.0, -0.5, 0.5, 0.0, 1.0, 0.5, -0.5, -1.0, -0.5, 0.5]
    data%y_points = [0.0, 0.0, 0.87, 0.87, 0.0, -0.86, -0.86, 0.0, 0.0, 0.87, 0.87, 0.0, -0.86, -0.86]
    data%z_points = [0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1]  

    data%cell_type =[13_I1P,13_I1P,13_I1P,13_I1P,13_I1P,13_I1P]
    data%offset = [6, 12, 18, 24, 30, 36] 
    
    allocate(data%connect(1:data%offset(data%num_cells)))
    data%connect = [0,1,2,7,8,9,&
    0,2,3,7,9,10,&
    0,3,4,7,10,11,&
    0,4,5,7,11,12,&
    0,5,6,7,12,13,&
    0,6,1,7,13,8] 

    data%esc =[0.0, 0.0, 0.1892, 0.1892, 0.0, 0.1849, 0.1849, 0.0, 0.0, 0.1892, 0.1892, 0.0, 0.1849, 0.1849]

    data%camp_x =[1.0, 1.0, -0.9178, -0.9178, 1.0, -0.9048, -0.9048, 1.0, 1.0, -0.9178, -0.9178, 1.0, -0.9048, -0.9048] 
    data%camp_y =[0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0] 
    data%camp_z =[-1.0, -1.0, 0.9178, 0.9178, -1.0, 0.9048, 0.9048, -1.0, -1.0, 0.9178, 0.9178, -1.0, 0.9048, 0.9048] 


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

end program ejemplo_prisma_hex
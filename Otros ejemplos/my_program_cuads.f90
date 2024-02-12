program my_program_cuads

    !---------------------------------------------------------------
    ! Modulos
    !---------------------------------------------------------------

    ! Modulos de la libreria VTK-Fortran
    use penf
    use vtk_fortran, only : vtk_file, pvtk_file

    ! Modulos propios
    use memoria
    use file_managment

    implicit none

    !----------------------------------------------------------------
    ! VARIABLES
    !----------------------------------------------------------------

    ! Definimos las variables........................................
    type(vtk_file)                      ::  a_vtk_file                                      ! Elemento con las funciones para crear VTKFiles
    type(pvtk_file)                     ::  a_pvtk_file                                     ! Elemento con las funciones para crear PVTKFiles
    type(Unstructured_data)             ::  data    
    integer(I4P)                        ::  error                                           ! Variable para guardar los errores de las funciones
    character(len = 40), dimension(2)   ::  filename_vtu                                    ! Nombre del archivo que vamos a crear
    
    !----------------------------------------------------------------
    ! CODIGO
    !----------------------------------------------------------------
        ! Variables comunes
    data%a_vtk_file = a_vtk_file
    data%a_pvtk_file = a_pvtk_file

    ! PRIMER ARCHIVO .vtu............................................

    data%num_cells   =   4_I4P
    data%num_points  =   9_I4P

    ! Reservamos memoria para las variable allocatable.
    call reservarPuntos(data%x_points, data%y_points, data%z_points, data%num_points)
    call reservarConnect(data%offset, data%cell_type, data%num_cells)
    call reservarCampo(data%camp_x, data%camp_y,data%camp_z, data%num_points)
    allocate(data%esc(1:data%num_points))

    ! Inicializacion de las variables 
    data%mesh_topology = 'UnstructuredGrid'   
    data%format = 'ascii'                                                  
    data%filename_vtu = 'XML_vtu_cuad_1.vtu'
    filename_vtu(1) =data%filename_vtu;


    data%x_points = [0, 1, 2, 0, 1, 2, 0, 1, 2]
    data%y_points = [0, 0, 0, -1, -1, -1, -2, -2, -2]
    data%z_points = [0, 0, 0, 0, 0, 0, 0, 0, 0]  

    data%cell_type =[9_I1P, 9_I1P, 9_I1P, 9_I1P] 
    data%offset = [4, 8, 12, 16] 
    
    allocate(data%connect(1:data%offset(data%num_cells)))
    data%connect = [0, 1, 4, 3, 1, 2, 5, 4, 3, 4, 7, 6, 4, 5, 8, 7] 

    data%esc =[0, 0, 0, 0, 1, 4, 0, 4, 16]

    data%camp_x =[1, -1, 1, -1, 1, -1, 1, -1, 1] 
    data%camp_y =[1, -1, 1, -1, 1, -1, 1, -1, 1] 
    data%camp_z =[0, 0, 0, 0, 0, 0, 0, 0, 0] 


    ! Creamos el archivo
    call check_error(data%open_vtu())
    call check_error(data%a_vtk_file%xml_writer%write_dataarray(data_name = 'Temperature', x=data%esc))
    call check_error(data%a_vtk_file%xml_writer%write_dataarray(data_name = 'Camp electrico real',&
        x=data%camp_x, y=data%camp_y, z=data%camp_z))
    
    call check_error(data%close_vtu())
 
    ! Liberamos memoria para las variables que cambian en el segundo archivo
    call libConnect(data%connect, data%offset, data%cell_type)


    ! SEGUNDO ARCHIVO .vtu...........................................

    ! Reservamos los arrays con los nuevos tamaños
    call reservarConnect(data%offset, data%cell_type, data%num_cells)

    ! Valores de las variables para el segundo archivo
    data%filename_vtu = 'XML_vtu_cuad_2.vtu'
    filename_vtu(2) = data%filename_vtu;
    data%num_cells = 6_I1P

    data%cell_type =[4_I1P, 4_I1P, 4_I1P, 4_I1P, 4_I1P, 4_I1P] 
    data%offset = [3,6,9,12,15,18] 
    
    allocate(data%connect(1:data%offset(data%num_cells)))
    data%connect = [0, 1, 2, 3, 4, 5, 6, 7, 8, 0, 3, 6, 1, 4, 7, 2, 5, 8] 

    ! Creamos el archivo
    call check_error(data%open_vtu())
    
    call check_error(data%a_vtk_file%xml_writer%write_dataarray(data_name = 'Temperature', x=data%esc))
    call check_error(data%a_vtk_file%xml_writer%write_dataarray(data_name = 'Camp electrico real',&
        x=data%camp_x, y=data%camp_y, z=data%camp_z))
    
    call check_error(data%close_vtu())

    ! Liberamos memoria...............................................
    call libConnect(data%connect, data%offset, data%cell_type)
    call libPuntos(data%x_points, data%y_points, data%z_points)
    call libEsc(data%esc)
    call libCampo(data%camp_x, data%camp_y,data%camp_z)

    ! ARCHIVO PVTU DONDE ALMACENAMOS LAS PIEZAS ......................

    ! Cambiamos las variables necesarias
    data%mesh_topology = 'PUnstructuredGrid'
    data%filename = 'XML_pvtu_cuad.pvtu'

    call check_error(data%crear_paralelo())

    ! Archivos fuente
    call check_error(data%a_pvtk_file%xml_writer%write_parallel_geo(source = filename_vtu(1)))
    call check_error(data%a_pvtk_file%xml_writer%write_parallel_geo(source = filename_vtu(2)))

    ! Cerrar archivo
    call check_error(data%a_pvtk_file%finalize())

    !----------------------------------------------------------------
    ! FUNCIONES Y SUBRUTINAS
    !----------------------------------------------------------------

    contains

    subroutine check_error(error)
        integer, intent(in) :: error
        if ( error /= 0 ) then
            print *, "ERROR ", error
        end if
        
    end subroutine check_error

end program my_program_cuads
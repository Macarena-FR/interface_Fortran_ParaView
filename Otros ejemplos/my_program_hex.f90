program my_program_hex

    !-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    ! Modulos
    !-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    
    ! Modulos de la libreria VTK-Fortran
    use penf
    use vtk_fortran, only : vtk_file, pvtk_file

    ! Modulos propios
    use memoria
    use file_managment

    implicit none

    !-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    ! VARIABLES
    !-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    
    ! Definimos las variables.................................................................................................................................................
    type(vtk_file)                      ::  a_vtk_file                                      ! Elemento con las funciones para crear VTKFiles
    type(pvtk_file)                     ::  a_pvtk_file                                     ! Elemento con las funciones para crear PVTKFiles
    type(Unstructured_data)             ::  data    
    integer(I4P)                        ::  error                                           ! Variable para guardar los errores de las funciones
    character(len = 40), dimension(2)   ::  filename_vtu                                    ! Nombre del archivo que vamos a crear
    
    !-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    ! CODIGO
    !-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    
    ! Variables comunes
    data%a_vtk_file = a_vtk_file
    data%a_pvtk_file = a_pvtk_file

    ! PRIMER ARCHIVO .vtu......................................................................................................................................................
    
    data%num_cells   =   6_I4P
    data%num_points  =   7_I4P

    ! Reservamos memoria para las variable allocatable.
    call reservarPuntos(data%x_points, data%y_points, data%z_points, data%num_points)
    call reservarConnect(data%offset, data%cell_type, data%num_cells)
    call reservarCampo(data%camp_x, data%camp_y,data%camp_z, data%num_points)
    allocate(data%esc(1:data%num_points))

    ! Inicializacion de las variables 
    data%mesh_topology = 'UnstructuredGrid'   
    data%format = 'ascii'                                                  
    data%filename_vtu = 'XML_vtu_hex_1.vtu'
    filename_vtu(1) =data%filename_vtu;


    data%x_points = [0.0, 1.0, 0.5, -0.5, -1.0, -0.5, 0.5]
    data%y_points = [0.0, 0.0, 0.87, 0.87, 0.0, -0.86, -0.86]
    data%z_points = [0, 0, 0, 0, 0, 0, 0]  

    data%cell_type =[5_I1P, 5_I1P, 5_I1P, 5_I1P, 5_I1P, 5_I1P] 
    data%offset = [3,6,9,12,15,18] 
    
    allocate(data%connect(1:data%offset(data%num_cells)))
    data%connect = [0,1,2,0,2,3,0,3,4,0,4,5,0,5,6,0,6,1] 

    data%esc =[0.0, 0.0, 0.189225, 0.189225, 0.0, 0.1849, 0.1849]

    data%camp_x =[1.0, -1.0, -0.3971, 0.3971, -1.0, -0.4258, 0.4258] 
    data%camp_y =[1.0, -1.0, -0.3971, 0.3971, -1.0, -0.4258, 0.4258] 
    data%camp_z =[0, 0, 0, 0, 0, 0, 0] 


    ! Creamos el archivo
    call check_error(data%open_vtu())
    call check_error(data%a_vtk_file%xml_writer%write_dataarray(data_name='Temperature', x=data%esc))
    call check_error(data%a_vtk_file%xml_writer%write_dataarray(data_name='Camp electrico real',&
                                                         x=data%camp_x, y=data%camp_y, z=data%camp_z))
    
   
    call check_error(data%close_vtu())
   
    !Liberamos memoria para las variables que cambian en el segundo archivo
    call libConnect(data%connect, data%offset, data%cell_type)


    ! SEGUNDO ARCHIVO .vtu....................................................................................
    
    ! Reservamos los arrays con los nuevos tamaños
    call reservarConnect(data%offset, data%cell_type, data%num_cells)

    ! Valores de las variables para el segundo archivo
    data%filename_vtu = 'XML_vtu_hex_2.vtu'
    filename_vtu(2) = data%filename_vtu;
    data%num_cells = 9_I1P

    data%cell_type =[4_I1P, 4_I1P, 4_I1P, 3_I1P, 3_I1P, 3_I1P, 3_I1P, 3_I1P, 3_I1P] 
    data%offset = [3,6,9,11,13,15,17,19,21] 
    
    allocate(data%connect(1:data%offset(data%num_cells)))
    data%connect = [1,0,4,3,0,6,2,0,5,1,2,2,3,3,4,4,5,5,6,6,1] 

    ! Creamos el archivo
    call check_error(data%open_vtu())
    
    call check_error(data%a_vtk_file%xml_writer%write_dataarray(data_name='Temperature', x=data%esc))
    call check_error(data%a_vtk_file%xml_writer%write_dataarray(data_name='Camp electrico real',&
                                     x=data%camp_x, y=data%camp_y, z=data%camp_z))
   
    call check_error(data%close_vtu())
   
    ! Liberamos memoria...............................................................................................................................
    call libConnect(data%connect, data%offset, data%cell_type)
    call libPuntos(data%x_points, data%y_points, data%z_points)
    call libEsc(data%esc)
    call libCampo(data%camp_x, data%camp_y,data%camp_z)
    

    ! ARCHIVO PVTU DONDE ALMACENAMOS LAS PIEZAS .........................................................................................................................
    
    ! Cambiamos las variables necesarias
    data%mesh_topology = 'PUnstructuredGrid'
    data%filename = 'XML_pvtu_hex.pvtu'

    call check_error(data%crear_paralelo())

    ! Archivos fuente
    call check_error(data%a_pvtk_file%xml_writer%write_parallel_geo(source = filename_vtu(1)))
    call check_error(data%a_pvtk_file%xml_writer%write_parallel_geo(source = filename_vtu(2)))

    ! Cerrar archivo
    call check_error(data%a_pvtk_file%finalize())

    !-----------------------------------------------------------------------------------------------------------------------
    ! FUNCIONES
    !-----------------------------------------------------------------------------------------------------------------------

    contains


    subroutine check_error(error)
        integer, intent(in) :: error
        if ( error /= 0 ) then
            print *, "ERROR ", error
        end if
        
    end subroutine check_error


    ! Compilar  ->  gfortran -I./VTKFortran/static/mod my_program_hex.f90 memoria.o file_managment.o -L./VTKFortran/static -l vtkfortran -o my_program_hex


end program my_program_hex
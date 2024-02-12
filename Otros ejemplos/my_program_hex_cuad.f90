program my_program_hex_cuad

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
    
    data%num_cells   =   15_I4P
    data%num_points  =   19_I4P

    ! Reservamos memoria para las variable allocatable.
    call reservarPuntos(data%x_points, data%y_points, data%z_points, data%num_points)
    call reservarConnect(data%offset, data%cell_type, data%num_cells)
    call reservarCampo(data%camp_x, data%camp_y,data%camp_z, data%num_points)
    allocate(data%esc(1:data%num_points))

    ! Inicializacion de las variables 
    data%mesh_topology = 'UnstructuredGrid'   
    data%format = 'ascii'                                                  
    data%filename_vtu = 'XML_vtu_hex_cuad.vtu'
    filename_vtu(1) =data%filename_vtu;


    data%x_points = [0.0, 1.0, 0.5, -0.5, -1.0, -0.5, 0.5, 0.75, 0.0, -0.76, -0.76, 0.0, 0.76, 0.5, 0.25, -0.25, &
    -0.5, -0.25, 0.25]
    data%y_points = [0.0, 0.0, 0.87, 0.87, 0.0, -0.86, -0.86, 0.435, 0.87, 0.44, -0.44, -0.87, -0.44, 0.0, 0.435, &
    0.435, 0.0, -0.435, -0.435]
    data%z_points = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]  

    data%cell_type =[22_I1P, 22_I1P, 22_I1P, 22_I1P, 22_I1P, 22_I1P, 4_I1P, 4_I1P, 4_I1P, 4_I1P, 4_I1P, 4_I1P, 4_I1P, 4_I1P, 4_I1P] 
    data%offset = [6, 12, 18, 24, 30, 36, 39, 42, 45, 48, 51, 54, 59, 64, 69] 
    
    allocate(data%connect(1:data%offset(data%num_cells)))
    data%connect = [0,1,2,13,7,14,&
    0, 2, 3, 14, 8, 15,& 
    0, 3, 4, 15, 9, 16,& 
    0, 4, 5, 16, 10, 17,&  
    0, 5, 6, 17, 11, 18,&  
    0, 6, 1, 18, 12, 13,& 
    1, 7, 2,&  
    2, 8, 3,&  
    3, 9, 4,&  
    4, 10, 5,&  
    5, 11, 6,& 
    6, 12, 1,&  
    1, 13, 0, 16, 4,&  
    3, 15, 0, 18, 6,&  
    2, 14, 0, 17, 5] 

    data%esc =[0.0, 0.0,0.189225, 0.189225, 0.0,0.1849, 0.1849, 0.1064390625, 0.0, 0.11182336, 0.11182336, 0.0, 0.11182336, 0.0,&
     0.0118265625, 0.0118265625, 0.0, 0.0118265625, 0.0118265625]

    data%camp_x =[1.0, -1.0, -0.3971, 0.3971, -1.0, -0.4258, 0.4258, -0.8358, -0.9178, 0.5358, -0.809, -0.9178, 0.5358, 0.0, &
    -0.549, 0.8358, 0.0, -0.549, 0.8358] 
    data%camp_y =[1.0, -1.0, -0.3971, 0.3971, -1.0, -0.4258, 0.4258, -0.8358, -0.9178, 0.5358, -0.809, -0.9178, 0.5358, 0.0, &
    -0.549, 0.8358, 0.0, -0.549, 0.8358] 
    data%camp_z =[0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] 


    ! Creamos el archivo
    call check_error(data%open_vtu())
    call check_error(data%a_vtk_file%xml_writer%write_dataarray(data_name='Temperature', x=data%esc))
    call check_error(data%a_vtk_file%xml_writer%write_dataarray(data_name='Camp electrico real',&
                                                         x=data%camp_x, y=data%camp_y, z=data%camp_z))
    
    call check_error(data%close_vtu())
 
    !Liberamos memoria para las variables que cambian en el segundo archivo
    call libConnect(data%connect, data%offset, data%cell_type)

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


    ! Compilar  ->  gfortran -I./VTKFortran/static/mod my_program_hex_cuad.f90 memoria.o file_managment.o -L./VTKFortran/static -l vtkfortran -o my_program_hex_cuad


end program my_program_hex_cuad
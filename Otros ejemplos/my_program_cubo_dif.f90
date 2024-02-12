program my_program_cubo_dif

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
    
    data%num_cells   =   40_I4P
    data%num_points  =   27_I4P

    ! Reservamos memoria para las variable allocatable.
    call reservarPuntos(data%x_points, data%y_points, data%z_points, data%num_points)
    call reservarConnect(data%offset, data%cell_type, data%num_cells)
    call reservarCampo(data%camp_x, data%camp_y,data%camp_z, data%num_points)
    allocate(data%esc(1:data%num_points))

    ! Inicializacion de las variables 
    data%mesh_topology = 'UnstructuredGrid'   
    data%format = 'ascii'                                                  
    data%filename_vtu = 'XML_vtu_cubo_dif.vtu'
    filename_vtu(1) =data%filename_vtu;


    data%x_points = [0.0, 0.0, 0.0, 0.5, 0.5, 0.5, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.5, 0.5, 0.5, 1.0, 1.0,&
     1.0, 0.0, 0.0, 0.0, 0.5, 0.5, 0.5, 1.0, 1.0, 1.0]
    data%y_points = [0.0, -0.5, -1.0, 0.0, -0.5, -1.0, 0.0, -0.5, -1.0, 0.0, -0.5, -1.0, 0.0, -0.5, -1.0,&
     0.0, -0.5, -1.0, 0.0, -0.5, -1.0, 0.0, -0.5, -1.0, 0.0, -0.5, -1.0]
    data%z_points = [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,&
     1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]  

    data%cell_type =[10_I1P, 10_I1P, 10_I1P,10_I1P,10_I1P,10_I1P, 10_I1P, 10_I1P,10_I1P,10_I1P,&
    10_I1P, 10_I1P, 10_I1P,10_I1P,10_I1P,10_I1P, 10_I1P, 10_I1P,10_I1P,10_I1P,&
    10_I1P, 10_I1P, 10_I1P,10_I1P,10_I1P,10_I1P, 10_I1P, 10_I1P,10_I1P,10_I1P,&
    10_I1P, 10_I1P, 10_I1P,10_I1P,10_I1P,10_I1P, 10_I1P, 10_I1P,10_I1P,10_I1P]
    data%offset = [4, 8, 12, 16, 20, 24, 28, 32, 36, 40,&
     44, 48, 52, 56, 60, 64, 68, 72, 76, 80,&
     84, 88, 92, 96, 100, 104, 108, 112, 116, 120,&
     124, 128, 132, 136, 140, 144, 148, 152, 156, 160] 
    
    allocate(data%connect(1:data%offset(data%num_cells)))
    data%connect = [0,10,12,9,&
    10,12,18,9,&
    0,4,12,3,&
    4,6,12,3,&
    6,12,16,15,&
    12,16,24,15,&
    4,6,16,7,&
    4,8,16,7,&
    0,4,10,1,&
    2,4,10,1,&
    2,4,14,5,&
    4,8,14,5,&
    14,16,26,17,&
    8,14,16,17,&
    10,14,20,11,&
    2,10,14,11,&
    14,22,26,23,&
    14,20,22,23,&
    10,20,22,19,&
    10,18,22,19,&
    12,18,22,21,&
    12,22,24,21,&
    16,22,24,25,&
    16,22,26,25,&
    4,12,16,6,&
    4,14,16,8,&
    4,10,12,0,&
    4,10,14,2,&
    12,16,22,24,&
    14,16,22,26,&
    10,12,22,18,&
    10,14,22,20,&
    4,14,16,13,&
    4,12,16,13,&
    4,10,12,13,&
    4,10,14,13,&
    12,16,22,13,&
    14,16,22,13,&
    10,14,22,13,&
    10,13,22,13] 

    data%esc =[0.0, 0.0, 0.0, 0.0, 0.0625, 0.25, 0.0, 0.25, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0625, 0.25, 0.0,&
     0.25, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0625, 0.25, 0.0, 0.25, 1.0]

    data%camp_x =[1, 0, -1, 0, 1, 0, -1, 0, 1, 1, 0, -1, 0, 1, 0, -1, 0, 1, 1, 0, -1, 0, 1, 0, -1, 0, 1] 
    data%camp_y =[1, 0, -1, 0, 1, 0, -1, 0, 1, 1, 0, -1, 0, 1, 0, -1, 0, 1, 1, 0, -1, 0, 1, 0, -1, 0, 1] 
    data%camp_z =[0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] 


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


    ! Compilar  ->  gfortran -I./VTKFortran/static/mod my_program_cuads_cuad.f90 memoria.o file_managment.o -L./VTKFortran/static -l vtkfortran -o my_program_cuads_cuad


end program my_program_cubo_dif
program ejemplo_cuadrado_cuadratico

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
    !---------------------------------------------------------------------
    
    ! Definimos las variables............................................
    type(vtk_file)                      ::  a_vtk_file                                      ! Elemento con las funciones para crear VTKFiles
    type(pvtk_file)                     ::  a_pvtk_file                                     ! Elemento con las funciones para crear PVTKFiles
    type(Unstructured_data)             ::  data    
    integer(I4P)                        ::  error                                           ! Variable para guardar los errores de las funciones
    character(len = 40), dimension(2)   ::  filename_vtu                                    ! Nombre del archivo que vamos a crear
    
    !---------------------------------------------------------------------
    ! CODIGO
    !---------------------------------------------------------------------
    
    ! Variables comunes
    data%a_vtk_file = a_vtk_file
    data%a_pvtk_file = a_pvtk_file

    ! PRIMER ARCHIVO .vtu.................................................
    
    data%num_cells   =   10_I4P
    data%num_points  =   25_I4P

    ! Reservamos memoria para las variable allocatable.
    call reservarPuntos(data%x_points, data%y_points, data%z_points, data%num_points)
    call reservarConnect(data%offset, data%cell_type, data%num_cells)
    call reservarCampo(data%camp_x, data%camp_y,data%camp_z, data%num_points)
    allocate(data%esc(1:data%num_points))

    ! Inicializacion de las variables 
    data%mesh_topology = 'UnstructuredGrid'   
    data%format = 'ascii'                                                  
    data%filename_vtu = 'XML_vtu_cuad_cuad.vtu'
    filename_vtu(1) =data%filename_vtu;


    data%x_points = [0.0, 1.0, 2.0, 0.0, 1.0, 2.0, 0.0, 1.0, 2.0, 0.5, 1.5, 0.0, 1.0, 2.0, 0.5, 1.5,0.0, 1.0, 2.0, 0.5, 1.5,&
    0.5, 1.5, 0.5, 1.5]
    data%y_points = [0.0,0.0,0.0, -1.0, -1.0, -1.0, -2.0, -2.0, -2.0,0.0,0.0, -0.5, -0.5, -0.5, -1.0, -1.0, -1.5, -1.5,&
     -1.5, -2.0, -2.0, -0.5, -0.5, -1.5, -1.5]
    data%z_points = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]  

    data%cell_type =[28_I1P, 28_I1P, 28_I1P, 28_I1P, 4_I1P, 4_I1P, 4_I1P, 4_I1P, 4_I1P, 4_I1P] 
    data%offset = [9,18,27,36,41,46,51,56,61,66] 
    
    allocate(data%connect(1:data%offset(data%num_cells)))
    data%connect = [0,1,4,3,9,12,14,11,21,1,2,5,4,10,13,15,12,22,3,4,7,6,14,17,19,16,23,4,5,8,7,15,18,&
    20,17,24,0,9,1,10,2,3,14,4,15,5,6,19,7,20,8,0,11,3,16,6,1,12,4,17,7,8,18,5,13,2] 

    data%esc =[0.0,0.0,0.0,0.0, 1.0, 4.0,0.0, 4.0, 16.0,0.0,0.0,0.0, 0.25, 1.0, 0.25, 2.25,0.0, 2.25, 9.0, 1.0, 9.0, 0.0625,&
     0.5625, 0.5625, 5.0625]

    data%camp_x =[1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0, 1.0, -1.0,&
     -1.0, 1.0] 
    data%camp_y =[1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0, 1.0, -1.0,&
     -1.0, 1.0] 
    data%camp_z =[0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0, 0.0] 


    ! Creamos el archivo
    call check_error(data%open_vtu())
    call check_error(data%a_vtk_file%xml_writer%write_dataarray(data_name='Temperature', x=data%esc))
    call check_error(data%a_vtk_file%xml_writer%write_dataarray(data_name='Camp electrico real',&
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

end program ejemplo_cuadrado_cuadratico
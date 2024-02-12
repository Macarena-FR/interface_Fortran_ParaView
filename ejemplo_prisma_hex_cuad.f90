program ejemplo_prisma_hex_cuad

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
    
    type(vtk_file)                      ::  a_vtk_file       ! Elemento con las funciones para crear VTKFiles
    type(pvtk_file)                     ::  a_pvtk_file      ! Elemento con las funciones para crear PVTKFiles
    type(Unstructured_data)             ::  data    
    integer(I4P)                        ::  error            ! Variable para guardar los errores de las funciones
    character(len = 40), dimension(2)   ::  filename_vtu     ! Nombre del archivo que vamos a crear
    
    !--------------------------------------------------------------------
    ! CODIGO
    !--------------------------------------------------------------------
    
    ! Variables comunes
    data%a_vtk_file = a_vtk_file
    data%a_pvtk_file = a_pvtk_file

    ! PRIMER ARCHIVO .vtu................................................
    
    data%num_cells   =   6_I4P
    data%num_points  =   45_I4P

    ! Reservamos memoria para las variable allocatable.
    call reservarPuntos(data%x_points, data%y_points, data%z_points, data%num_points)
    call reservarConnect(data%offset, data%cell_type, data%num_cells)
    call reservarCampo(data%camp_x, data%camp_y,data%camp_z, data%num_points)
    allocate(data%esc(1:data%num_points))

    ! Inicializacion de las variables 
    data%mesh_topology = 'UnstructuredGrid'
    data%format = 'ascii'                                                  
    data%filename_vtu = 'XML_vtu_hex_3d_cuad.vtu'
    filename_vtu(1) =data%filename_vtu;

    data%x_points = [0.0, 1.0, 0.5, -0.5, -1.0, -0.5, 0.5, 0.0, 1.0, 0.5, -0.5, -1.0, -0.5, 0.5, 0.5, 0.25,&
	 -0.25, -0.5, -0.25, 0.25, 0.0, -0.76, -0.76, 0.0, 0.76, 0.76, 0.5, -0.5, -1.0, -0.5, 0.5, 1.0, 0.76,&
	  0.0, -0.76, -0.76, 0.0, 0.76, 0.5, 0.25, -0.25, -0.5, -0.25, 0.25, 0.0]
    data%y_points = [0.0, 0.0, 0.87, 0.87, 0.0, -0.86, -0.86, 0.0, 0.0, 0.87, 0.87, 0.0, -0.86, -0.86, 0.0,&
	 0.435, 0.435, 0.0, -0.43, -0.43, 0.87, 0.43, -0.43, -0.87, -0.43, 0.43, 0.87, 0.87, 0.0, -0.86, -0.86,&
	  0.0, 0.43, 0.87, 0.43, -0.43, -0.87, -0.43, 0.0, 0.435, 0.435, 0.0, -0.43, -0.43, 0.0]
    data%z_points = [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0,&
	 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,&
	  1.0, 1.0, 1.0, 1.0, 1.0, 0.5]  

    data%cell_type =[26_I1P, 26_I1P, 26_I1P, 26_I1P, 26_I1P, 26_I1P] 
    data%offset = [15,30,45,60,75,90] 
    
    allocate(data%connect(1:data%offset(data%num_cells)))
    data%connect = [0,1,2,7,8,9,14,25,15,38,32,39,44,31,26,&
    0,2,3,7,9,10,15,20,16,39,33,40,44,26,27,&
    0,3,4,7,10,11,16,21,17,40,34,41,44,27,28,&
    0,4,5,7,11,12,17,22,18,41,35,42,44,28,29,&
    0,5,6,7,12,13,18,23,19,42,36,43,44,29,30,&
    0,6,1,7,13,8,19,24,14,43,37,38,44,30,31] 

    data%esc =[0.0, 0.0, 0.1892, 0.1892, 0.0, 0.1849, 0.1849, 0.0, 0.0, 0.1892, 0.1892, 0.0, 0.1849, 0.1849, 0.0, 0.0118, 0.0118, 0.0,&
     0.0116, 0.0116, 0.0, 0.1068, 0.1068, 0.0, 0.1068, 0.1068, 0.1892, 0.1892, 0.0, 0.1849, 0.1849, 0.0, 0.1068, 0.0, 0.1068, 0.1068,&
      0.0, 0.1068, 0.0, 0.0118, 0.0118, 0.0, 0.0116, 0.0116, 0.0]

    data%camp_x =[1.0, 1.0, -0.9178, -0.9178, 1.0, -0.9048, -0.9048, 1.0, 1.0, -0.9178, -0.9178, 1.0, -0.9048, -0.9048, 1.0, 0.2028, 0.2028,&
     1.0, 0.2181, 0.2181, -0.9178, 0.2181, 0.2181, -0.9178, 0.2181, 0.2181, -0.9178, -0.9178, 1.0, -0.9048, -0.9048, 1.0, 0.2181, -0.9178,&
      0.2181, 0.2181, -0.9178, 0.2181, 1.0, 0.2028, 0.2028, 1.0, 0.2181, 0.2181, 1.0] 
    data%camp_y =[0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,&
     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0] 
    data%camp_z =[-1.0, -1.0, 0.9178, 0.9178, -1.0, 0.9048, 0.9048, -1.0, -1.0, 0.9178, 0.9178, -1.0, 0.9048, 0.9048, -1.0, -0.2028, -0.2028,&
     -1.0, -0.2181, -0.2181, 0.9178, -0.2181, -0.2181, 0.9178, -0.2181, -0.2181, 0.9178, 0.9178, -1.0, 0.9048, 0.9048, -1.0, -0.2181, 0.9178,&
      -0.2181, -0.2181, 0.9178, -0.2181, -1.0, -0.2028, -0.2028, -1.0, -0.2181, -0.2181, -1.0]

    ! Creamos el archivo
    call check_error(data%open_vtu())
    call check_error(data%a_vtk_file%xml_writer%write_dataarray(data_name ='Temperature', x=data%esc))
    call check_error(data%a_vtk_file%xml_writer%write_dataarray(data_name ='Camp electrico real',&
      x=data%camp_x, y=data%camp_y, z=data%camp_z))
    
    call check_error(data%close_vtu())
 
    !Liberamos memoria para las variables que cambian en el segundo archivo
    call libConnect(data%connect, data%offset, data%cell_type)

    !-----------------------------------------------------------------
    ! FUNCIONES
    !------------------------------------------------------------------
    
    contains

    subroutine check_error(error)
        integer, intent(in) :: error
        if ( error /= 0 ) then
            print *, "ERROR ", error
        end if
    end subroutine check_error

end program ejemplo_prisma_hex_cuad
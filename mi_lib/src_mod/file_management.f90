module file_management

    use penf
    use vtk_fortran, only : vtk_file, pvtk_file

    implicit none

    private
        
    type, public :: Unstructured_data

        type(vtk_file)                      ::  a_vtk_file     
        type(pvtk_file)                     ::  a_pvtk_file                                     ! Elemento con las funciones para crear VTKFiles
        character(len = 20)                 ::  mesh_topology, format                           ! Variables para definir el tipo malla y el formato de los datos
        character(len = 40)                 ::  filename_vtu, filename                          ! Nombre del archivo que vamos a crear
        integer(I4P)                        ::  num_cells, num_points                           ! Numero de celdas y puntos de la pieza
        real(R4P), allocatable              ::  x_points(:), y_points(:), z_points(:)           ! Vectores para definir las coordenadas de los puntos
        integer(I4P), allocatable           ::  connect(:), offset(:)                           ! Vectores donde guardaremos la info de las celdas de la pieza
        integer(I1P), allocatable           ::  cell_type(:)                                    ! Vector para el tipo de celda
        real(R8P), allocatable              ::  esc(:), camp_x(:), camp_y(:), camp_z(:)         ! Vectores para los datos escalares y vectoriales
    
    contains
        procedure   ::  open_vtu
        procedure   ::  close_vtu
        procedure   ::  crear_paralelo
    
    end type Unstructured_data
    
contains

    function crear_paralelo(self) result(error)

        class(Unstructured_data), intent(inout)    ::  self
        integer :: error

        ! Inicializamos archivo
        error = self%a_pvtk_file%initialize(filename = self%filename, mesh_topology = self%mesh_topology, mesh_kind = "Float32")
        if ( error /= 0 ) then
            return
        end if
        ! Abrimos PPonint Data 
        error = self%a_pvtk_file%xml_writer%write_dataarray(location = 'node', action = 'open')
        if ( error /= 0 ) then
            return
        end if
        ! temp
        error = self%a_pvtk_file%xml_writer%write_parallel_dataarray(data_name = 'Temperature',&
            data_type = 'Float64', number_of_components =1)

        if ( error /= 0 ) then
            return
        end if
        ! camp
        error = self%a_pvtk_file%xml_writer%write_parallel_dataarray(data_name = 'Camp electrico real',&
            data_type = 'Float64', number_of_components = 3)

        if ( error /= 0 ) then
            return
        end if

        ! Cerramos PPonint Data 
        error = self%a_pvtk_file%xml_writer%write_dataarray(location = 'node', action = 'close')

        if ( error /= 0 ) then
            return
        end if
        
    end function crear_paralelo

! Esta funcion crea el archivo vtu hasat abrir la seccion de dataPoint
    function open_vtu(self) result(error)
        class(Unstructured_data), intent(inout)    ::  self
        integer (I4P)       ::       error

        ! Inicializamos el archivo de salida 
        error = self%a_vtk_file%initialize(format=self%format, filename=self%filename_vtu,&
            mesh_topology=self%mesh_topology)
    
        if ( error /= 0 ) then
            return
        end if

        ! Añadimos las piezas al archivo

        if(self%mesh_topology == "UnstructuredGrid") then
            error = self%a_vtk_file%xml_writer%write_piece(np = self%num_points, nc = self%num_cells)
            if ( error /= 0 ) then
                return
            end if
        end if

        ! Añadimos los puntos, eso se hace añadiendo la geometria
        error = self%a_vtk_file%xml_writer%write_geo(np = self%num_points, nc = self%num_cells,&
            x = self%x_points,y = self%y_points, z = self%z_points)
        
        if ( error /= 0 ) then
            return
        end if

        ! Añadimos las celdas definiendo la conectividad, el tipo...
        error = self%a_vtk_file%xml_writer%write_connectivity(nc=self%num_cells, connectivity=self%connect,&
            offset=self%offset, cell_type=self%cell_type)
        
        if ( error /= 0 ) then
            return
        end if

        ! Abrimos seccion DataPoints
        error = self%a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
        
        if ( error /= 0 ) then
            return
        end if

    end function open_vtu 
        
    function close_vtu(self) result(error)
        class(Unstructured_data), intent(inout)    ::  self
        integer (I4P)       ::       error

        error = self%a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
        
        if ( error /= 0 ) then
            return
        end if

        ! Cerramos el bloque de la pieza
        error = self%a_vtk_file%xml_writer%write_piece()
        
        if ( error /= 0 ) then
            return
        end if
        
        ! Cerramos archivo
        error = self%a_vtk_file%finalize()

        if ( error /= 0 ) then
            return
        end if

        ! error = a_vtk_file%free()

        if ( error /= 0 ) then
            return
        end if
    end function close_vtu
     
end module file_management
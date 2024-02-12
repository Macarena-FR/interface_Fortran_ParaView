# Directorios
SRCDIR=mi_lib/src_mod
OBJDIR=mi_lib/objects
MODDIR=mi_lib/mod
LIBDIR=VTKFortran/static
LIBMODDIR=VTKFortran/static/mod
EXEDIR=exe
RESULTDIR=result

# Opciones de compilacion
COMPILER=ifort
FFLAGS=-I$(LIBMODDIR)
MODFLAGS=-module $(MODDIR)
LFLAGS=-L$(LIBDIR) -lvtkfortran

# Lista archivos fuente de los modulos
SOURCES=$(wildcard $(SRCDIR)/*.f90)
#Creacion lista .o
OBJECTS=$(patsubst $(SRCDIR)/%.f90,$(OBJDIR)/%.o,$(SOURCES))

# Lista archivos fuente de los modulos
SOURCES_EXE=$(wildcard *.f90)
#Creacion lista .o
EXE=$(patsubst %.f90,$(EXEDIR)/%,$(SOURCES_EXE))



all: directories compile-vtkfortran $(OBJECTS) $(EXE) run

# Regla para crear directorios necesarios
directories:
	@mkdir -p $(OBJDIR) $(MODDIR) $(EXEDIR) $(RESULTDIR)

# Regla para compilar VTKFortran
compile-vtkfortran:
	$(MAKE) -C VTKFortran COMPILER=intel

# Compilación de los módulos de mi librería
$(OBJDIR)/%.o: $(SRCDIR)/%.f90
	@echo "[INFO] Compilando el módulo: $<..."
	@$(COMPILER) $(MODFLAGS) -c $< -o $@ $(FFLAGS) $(LFLAGS)  -diag-disable=10448
	@echo

# Compilar ejecutables
$(EXEDIR)/%: %.f90 $(OBJECTS)
	@echo "[INFO] Compilando el ejecutable: $<..."
	@$(COMPILER) -I$(MODDIR) $< $(OBJECTS) -o $@ $(FFLAGS) $(LFLAGS) -diag-disable=10448
	@echo

# Ejecutar los ejecutables
run: $(EXE)
	@for exe in $(EXE); do \
		echo "Ejecutando $$exe..."; \
		./$$exe; \
	done

	@mv *.vtu $(RESULTDIR)/ 2>/dev/null; \

	@mv *.pvtu $(RESULTDIR)/ 2>/dev/null; \

clean-exe:
	@rm -rf $(RESULTDIR)

clean:
	@rm -rf $(OBJDIR) $(MODDIR) $(EXE)
	$(MAKE) clean-exe

# Borrar todo
clean-all:
	$(MAKE) clean
	$(MAKE) -C VTKFortran cleanall
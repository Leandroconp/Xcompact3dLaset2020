FC = gfortran
OPTFC = -O3 -mcmodel=medium -funroll-loops -floop-optimize 
 ediss : module_param.o dissipation.o schemas.o derive.o parametre.o \

	$(FC)   $(OPTFC) -o ediss module_param.o dissipation.o schemas.o derive.o parametre.o \

module_param.o : module_param.f90
	$(FC)  $(OPTFC) -c module_param.f90
dissipation.o : dissipation.f90
	$(FC)  $(OPTFC) -c dissipation.f90
schemas.o : schemas.f90
	$(FC)  $(OPTFC) -c schemas.f90
derive.o : derive.f90
	$(FC)  $(OPTFC) -c derive.f90
parametre.o : parametre.f90
	$(FC)  $(OPTFC) -c parametre.f90

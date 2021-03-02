FC = gfortran
OPTFC = -O3 -mcmodel=medium -funroll-loops -floop-optimize 
 qcriterion : module_param.o critq.o schemas.o derive.o parametre.o patch.o \

	$(FC)   $(OPTFC) -o qcriterion module_param.o critq.o schemas.o derive.o parametre.o patch.o \

module_param.o : module_param.f90
	$(FC)  $(OPTFC) -c module_param.f90
critq.o : critq.f90
	$(FC)  $(OPTFC) -c critq.f90
schemas.o : schemas.f90
	$(FC)  $(OPTFC) -c schemas.f90
derive.o : derive.f90
	$(FC)  $(OPTFC) -c derive.f90
parametre.o : parametre.f90
	$(FC)  $(OPTFC) -c parametre.f90
patch.o : patch.f90
	$(FC)  $(OPTFC) -c patch.f90

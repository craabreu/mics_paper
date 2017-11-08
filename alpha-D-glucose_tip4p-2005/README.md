# alpha-D-Glucose

----------------------------------------------------------------------------------------------------
Requirements
----------------------------------------------------------------------------------------------------

1. [AMBER Tools](http://ambermd.org/#AmberTools) installed
2. Environmental variable AMBERHOME containing the path to AMBER Tools' directory
3. [LAMMPS](https://github.com/lammps/lammps) installed and compiled
4. Environmental variable LAMMPSHOME containing the path to the LAMMPS executable `lmp_mpi`

----------------------------------------------------------------------------------------------------
Preparation Steps
----------------------------------------------------------------------------------------------------

Create file `GLYCAM_06j.playmol` from AMBER's `GLYCAM_06j.dat` using playmoltools:

    playmoltools -f amber -i $AMBERHOME/dat/leap/parm/GLYCAM_06j.dat -o GLYCAM_06j.playmol

Create file `alpha-D-glucose.pdb` using Glycam-Web's [Carbohydrate Builder](http://glycam.org/cb).
For this, use the condensed code `DGlcpa1-OH` to generate a PDB file, download it, and execute:

    mv 1.pdb alpha-D-glucose.pdb

Create file `alpha-D-glucose.playmol` from `alpha-D-glucose.pdb` using playmoltools and GLYCAM's
prep file:

    playmoltools -f pdb -p $AMBERHOME/dat/leap/prep/GLYCAM_06j-1.prep -i alpha-D-glucose.pdb -o alpha-D-glucose.playmol

----------------------------------------------------------------------------------------------------
Execution Step
----------------------------------------------------------------------------------------------------

Run Playmol to create a system containing one glucose molecule solvated in water:

    playmol alpha-D-glucose_tip4p-2005.playmol

Execute LAMMPS to simulate the system:

    mpirun -n 4 $LAMMPSHOME/lmp_mpi -in in.lammps


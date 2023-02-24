#!/bin/bash                                                                                                                                               
#: Title       : build-debug.sh                                                                                                                           
#: Date        : 2023-02-23                                                                                                                               
#: Author      : Kjetil Kristoffer Solberg <post@ikjetil.no>                                                                                              
#: Version     : 1.0                                                                                                                                      
#: Description : Builds multiplication table (mult).                                                                                                                            
echo "Compiling multiplication table (mult) ..."
echo "> using release build <"

gfortran ./mult.f90 -o mult
if [[ $? -eq 0 ]]
then
    echo "> mult build ok <"
else
    echo "> mult build error <"
fi

echo "> build process complete <"

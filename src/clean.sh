#!/bin/bash                                                                                                                                               
#: Title       : build-debug.sh                                                                                                                           
#: Date        : 2023-02-23                                                                                                                               
#: Author      : Kjetil Kristoffer Solberg <post@ikjetil.no>                                                                                              
#: Version     : 1.0                                                                                                                                      
#: Description : Builds multiplication table (mult).                                                                                                                            
echo "> cleaning mult ... <"
rm ./mult 2> /dev/null
rm ./mult_util.mod 2> /dev/null
echo "> clean complete <"
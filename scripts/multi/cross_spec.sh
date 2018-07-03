#!/bin/bash

for var1 in FLNS FSNS LHFLX PSL SHFLX TMQ TREFHT U10; 
do
	for var2 in FLNS FSNS LHFLX PSL SHFLX TMQ TREFHT U10;
	do
		Rscript cross_spec.R $var1 $var2 $1 $2 $3 $4 $5
	done
done

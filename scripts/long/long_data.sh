#!/bin/bash

for var in FLNS FSNS LHFLX PSL SHFLX TMQ TREFHT U10; 
do
	Rscript long_data.R $var $1 $2 $3
done

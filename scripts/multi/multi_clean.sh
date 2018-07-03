#!/bin/bash

for var in FLNS FSNS LHFLX PSL SHFLX TMQ TREFHT U10; 
do
		Rscript multi_clean.R $var $1 $2 $3 $4 $5
done

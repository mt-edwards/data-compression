#!/bin/bash

for var1 in TMQ TREFHT U10; 
do
	for var2 in TMQ TREFHT U10;
	do
		Rscript multi_model.R $var1 $var2 $1 $2 $3 $4 
	done
done

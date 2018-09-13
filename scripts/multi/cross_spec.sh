#!/bin/bash

for var1 in TMQ TS U10; 
do
	for var2 in TMQ TS U10;
	do
		Rscript cross_spec.R $var1 $var2 $1 $2 $3 $4
	done
done

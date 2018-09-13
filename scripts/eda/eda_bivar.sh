#!/bin/bash

for var1 in TMQ TS U10; 
do
	for var2 in TMQ TS U10;
	do
		Rscript eda_bivar.R $var1 $var2
	done
done
#!/bin/bash

for var in TMQ TREFHT U10; 
do
	Rscript long_model.R $var $1 $2 $3 $4
done

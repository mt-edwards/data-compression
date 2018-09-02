#!/bin/bash

for var in TMQ TREFHT U10; 
do
	Rscript lat_select.R $var $1 $2 $3 $4
done

#!/bin/bash

for var in TMQ TREFHT U10; 
do
	Rscript temp_data.R $var $1 $2 $3
done

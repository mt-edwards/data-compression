#!/bin/bash

for var in TMQ TREFHT U10; 
do
	Rscript long_select.R $var $1 $2 $3
done

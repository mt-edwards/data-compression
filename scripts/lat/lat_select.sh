#!/bin/bash

for var in TMQ TS U10; 
do
	Rscript lat_select.R $var $1 $2 $3 $4
done

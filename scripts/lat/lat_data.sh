#!/bin/bash

for var in TMQ TS U10; 
do
	Rscript lat_data.R $var $1 $2 $3
done

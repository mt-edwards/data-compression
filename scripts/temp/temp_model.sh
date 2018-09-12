#!/bin/bash

for var in TMQ TS U10; 
do
	Rscript temp_model.R $var $1 $2 $3
done

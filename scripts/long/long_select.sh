#!/bin/bash

for var in TMQ TS U10; 
do
	Rscript long_select.R $var $1 $2 $3
done

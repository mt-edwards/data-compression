#!/bin/bash

for var in TMQ TS U10; 
do
	Rscript pgram_plot.R $var $1 $2 $3
done

#!/bin/bash

for var1 in TMQ TS U10; 
do

	Rscript sim_univar.R $var1 $1 $2 $3 $4 $5

done

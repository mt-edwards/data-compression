#!/bin/bash

for var1 in TMQ TREFHT U10; 
do

	Rscript diag_univar.R $var1 $1 $2 $3 $4 $5

done

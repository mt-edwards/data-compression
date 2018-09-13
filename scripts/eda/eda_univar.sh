#!/bin/bash

for var in TMQ TS U10; 
do
	Rscript eda_univar.R $var
done

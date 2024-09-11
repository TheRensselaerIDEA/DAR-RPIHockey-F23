#!/bin/bash
sed -i -e '$aCXX = g++ -std=gnu++14' ~/.R/Makevars
R -e 'install.packages("ranger",repos="https://cloud.r-project.org/")'
sed '$d' ~/.R/Makevars

set terminal png
set output "plot.png"

set view equal xyz
# set view 10,30

set dgrid3d 20,20
set pm3d
splot "datafile.dat" u 1:2:3 with lines

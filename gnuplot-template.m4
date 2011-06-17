dnl Template script, uses macros DATE and COLOUR
# gnuplot command script for DATE

set terminal postscript enhanced color solid "Times-Roman" 20 eps size 6.4,4.8

# lots of samples
set samples 1001

unset key

set output "DATE-ele.ps"
set xlabel "Distance (km)"
set ylabel "Elevation (m above s.l.)"
plot "DATE.dat" using 4:2 with lines linetype rgb "COLOUR"

set output "DATE-spd.ps"
set xdata time
set timefmt "%H:%M:%S"
set xlabel "Time (HH:MM)"
set ylabel "Speed (km/h)"
plot "DATE.dat" using 1:3 with lines linetype rgb "COLOUR"

set terminal postscript enhanced color solid "Times-Roman" 20 eps size 8,2
set output "DATE-ele2.ps"
unset xdata
unset xlabel
unset ylabel
set xtics axis nomirror
set ytics axis nomirror
set zeroaxis
unset x2tics
unset y2tics
unset border
set xrange [0:100]
set yrange [-100:1000]
set style fill solid
set size 1,1
plot "DATE.dat" using 4:2 with filledcurves y1=0 linetype rgb "COLOUR"

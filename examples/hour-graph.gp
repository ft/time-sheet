reset
set terminal pdf size 24cm,12cm
set xlabel "Jahr.Woche"
set ylabel "Stunden"
set grid
set boxwidth 0.55 relative
set style data histograms
set style histogram rowstacked
set style fill transparent solid 0.7 solid border -1
set xtics rotate
set xtics norangelimit

set output "hours.pdf"
plot "hours.txt" using 4:xtic(1) with boxes lc rgb"green" title "Ist", \
     ""          using 3 with boxes lc rgb"blue" title "Soll", \
     ""          using 5 with boxes lc rgb"red" title "Differenz"

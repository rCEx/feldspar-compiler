set term pdf enhanced color
set output "dot.pdf"

lg(x) = log(x) / log(2)

set key left
set logscale y
set xrange [10:24]
set xtics 1
set grid
set ylabel "Wall clock time (ms)"
set xlabel "2^x elements"

set title "Dot Product"
plot "feldspar/dotFeldspar.log" using (lg($2)):($1/1e6) title 'Feldspar -O3' with linespoints,\
     "pire/dotPIRE.log"         using (lg($2)):($1/1e6) title 'PIRE -O3'     with linespoints

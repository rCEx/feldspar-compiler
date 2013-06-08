set term pdf enhanced color
set output "bitonic.pdf"

lg(x) = log(x) / log(2)

set key left
set logscale y
set xrange [10:24]
set xtics 1
set grid
set ylabel "Wall clock time (ms)"
set xlabel "2^x elements"

set title "Bitonic sort"
plot "feldspar/bitonicFeldspar.log" using (lg($2)):($1/1e6) title 'Feldspar -O3' with linespoints,\
     "pire/bitonicPIRE.log"         using (lg($2)):($1/1e6) title 'PIRE -O3'     with linespoints,\
     "ref.log"                      using (lg($2)):($1/1e6) title 'Reference'     with linespoints



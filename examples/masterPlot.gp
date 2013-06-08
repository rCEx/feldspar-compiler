set term pdf enhanced color
set output "allPlots.pdf"

lg(x) = log(x) / log(2)

set key left
set logscale y
set xrange [10:24]
set xtics 1
set grid
set ylabel "Wall clock time (ms)"
set xlabel "2^x elements"

set title "Scan"
plot "scan/feldspar/scanFeldspar.log" using (lg($2)):($1/1e6) title 'Feldspar -O3' with linespoints,\
     "scan/pire/scanPIRE.log"         using (lg($2)):($1/1e6) title 'PIRE -O3'     with linespoints,\
     "scan/ref.log"                   using (lg($2)):($1/1e6) title 'Reference'     with linespoints


set title "Bitonic sort"
plot "bitonic/feldspar/bitonicFeldspar.log" using (lg($2)):($1/1e6) title 'Feldspar -O3' with linespoints,\
     "bitonic/pire/bitonicPIRE.log"         using (lg($2)):($1/1e6) title 'PIRE -O3'     with linespoints,\
     "bitonic/ref.log"                      using (lg($2)):($1/1e6) title 'Reference'     with linespoints


set title "Dot Product"
plot "dot/feldspar/dotFeldspar.log" using (lg($2)):($1/1e6) title 'Feldspar -O3' with linespoints,\
     "dot/pire/dotPIRE.log"         using (lg($2)):($1/1e6) title 'PIRE -O3'     with linespoints

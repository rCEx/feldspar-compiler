set term pdf enhanced color
set output "scan.pdf"

lg(x) = log(x) / log(2)

set key left
set logscale y
set xrange [10:24]
set xtics 1
set grid
set ylabel "Wall clock time (ms)"
set xlabel "2^x elements"

set title "Scan"
plot "feldspar/scanFeldspar.log" using (lg($2)):($1/1e6) title 'Feldspar -O3' with linespoints,\
     "pire/scanPIRE.log"         using (lg($2)):($1/1e6) title 'PIRE -O3'     with linespoints,\
     "ref.log"                   using (lg($2)):($1/1e6) title 'Reference'     with linespoints

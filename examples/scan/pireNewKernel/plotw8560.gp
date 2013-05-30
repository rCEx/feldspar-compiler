set term pdf enhanced color
set output "logs.pdf"

lg(x) = log(x) / log(2)

set key left
set logscale y
set xrange [1:24]
set xtics 1
set grid
set ylabel "Wall clock time (ms)"
set xlabel "2^x elements"

set title "Scan (EliteBook 8560w)"
plot "../feldspar/scanFeldspar.log" using (lg($2)):($1/1e6) title 'Feldspar' with linespoints,\
     "scanPIRE1024.log" using (lg($2)):($1/1e6) title 'Local item size 1024' with linespoints,\
     "scanPIREfor.log" using (lg($2)):($1/1e6) title 'old kernel (LIS 1024)' with linespoints


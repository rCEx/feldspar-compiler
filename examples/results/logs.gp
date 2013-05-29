set term pdf enhanced color
set output "logs.pdf"

lg(x) = log(x) / log(2)

set key left
set logscale y
set xrange [10:24]
set xtics 1
set grid
set ylabel "Wall clock time (ms)"
set xlabel "2^x elements"

set title "bitonic sort (MacBookPro)"
plot "mbp/bitonic/pire/baseline.log"            using (lg($2)):($1/1e6) title 'Baseline -O3' with linespoints,\
     "mbp/bitonic/feldspar/bitonicFeldspar.log" using (lg($2)):($1/1e6) title 'Feldspar -O3' with linespoints,\
     "mbp/bitonic/pire/bitonicPIRE.log"         using (lg($2)):($1/1e6) title 'PIRE -O3'     with linespoints,\
     "mbp/bitonic/pire/bitonicTweaked.log"      using (lg($2)):($1/1e6) title 'Tweaked -O3'  with linespoints

set title "bitonic sort (EC2)"
plot "ec2/bitonic/feldspar/bitonicFeldspar.log" using (lg($2)):($1/1e6) title 'Feldspar -O3' with linespoints,\
     "ec2/bitonic/pire/bitonicPIRE.log"         using (lg($2)):($1/1e6) title 'PIRE -O3'     with linespoints,\
     "ec2/bitonic/pire/bitonicTweaked.log"      using (lg($2)):($1/1e6) title 'Tweaked -O3'  with linespoints,\
     "ec2/bitonic/ref.log"                      using (lg($2)):1        title 'NVIDIA'       with linespoints

set title "scan (EC2)"
plot "ec2/scan/feldspar/scanFeldspar.log" using (lg($2)):($1/1e6) title 'Feldspar -O3' with linespoints,\
     "ec2/scan/pire/scanPIRE.log"         using (lg($2)):($1/1e6) title 'PIRE -O3'     with linespoints,\
     "ec2/scan/ref.log"                   using (lg($2)):1        title 'NVIDIA'       with linespoints

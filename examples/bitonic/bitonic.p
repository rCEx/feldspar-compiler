set term pdf enhanced color
set output "bitonic.pdf"

set key above
unset logscale

plot "feldspar/bitonicFeldspar.log" using 2:1 title 'Feldspar' with linespoints,\
     "pire/bitonicPIRE.log" using 2:1 title 'PIRE' with linespoints,\
     "pire/bitonicTweaked.log" using 2:1 title 'Tweaked' with linespoints,\
     "pire/baseline.log" using 2:1 title 'Baseline' with linespoints


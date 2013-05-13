

main :: IO ()
main = undefined

-- 1 input vector - 1 output vector
in1VecOut1Sca :: String
in1VecOut1Sca = undefined 


-- 2 input vectors - 1 output scalar
in2VecOut1Sca :: String
in2VecOut1Sca = undefined 


timeMeasure :: String
timeMeasure = "void outputMeasure(char *to, time_t time) {\n" ++
              "  FILE *fp = fopen(to, \"a\");\n"              ++
              "  if(fp != NULL) {\n"                          ++
              "    fprintf(fp, \"%li\\n\", time);\n"          ++
              "  }\n"                                         ++
              "  fclose(to);\n"                               ++
              "}\n"

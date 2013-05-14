import System.Cmd

main :: IO ()
main = do system cc
          sequence_ $ map system $ runAll "./a.out " 22

dotProd = appendFile "main.c" $ timeMeasure ++ main2VecSca "timersDotProd.log"

runAll name n = map ((++) name . show) (sizes n)

cc = "gcc -std=c99 -I ../C_new/ -I /usr/local/cuda/include/ -lOpenCL ../C_new/feldspar_c99.c -lm main.c"
sizes :: Integer -> [Integer]
sizes n = [2^i | i <- [0..n]]

timeMeasure :: String
timeMeasure = "void outputMeasure(char *to, clock_t time, int size) {\n" ++
              "  FILE *fp = fopen(to, \"a\");\n"              ++
              "  if(fp != NULL) {\n"                          ++
              "    fprintf(fp, \"%li %f %i\\n\", time, ((double)time)/CLOCKS_PER_SEC, size);\n" ++
              "  }\n"                                         ++
              "  fclose(fp);\n"                               ++
              "}\n"

main2VecSca :: String -> String
main2VecSca timerLog = 
  "int main (int argc, char *argv[]) {\n" ++
  "  const int size = atoi(argv[1]);\n" ++
  "  int *a = (int*) malloc(sizeof(int)*size);\n" ++
  "  int *b = (int*) malloc(sizeof(int)*size);\n" ++
  "  int i;\n" ++
  "  for(i = 0; i < size; i++) {\n" ++
  "    a[i] = i%4;\n" ++
  "    b[i] = i%4;\n" ++
  "  }\n" ++
  "  int *res;\n" ++
  "  clock_t t;\n" ++
  "  t = clock();\n" ++
  "  f0(a,size,b,size, &res);\n" ++
  "  t = clock() - t;\n" ++
  "  outputMeasure(" ++ "\"" ++ timerLog ++ "\",t, size);\n" ++
  "  printf(\"%i : res \\n\", res[0]);\n" ++
  "  return 0;\n" ++
  "}\n"



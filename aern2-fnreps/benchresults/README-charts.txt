* Regenerate results.csv:

rm results.csv
bash runBench.sh results.csv

* Generate svg execution time charts:

aern2-bench-chart FnOp FnRepr Accuracy "LinFromTo 0 100" ExecTime "LogFromTo 0.1 1000" results.csv charts

* Generate svg memory usage charts:

aern2-bench-chart FnOp FnRepr Accuracy "LinFromTo 0 100" MaxMem "LogFromTo 5000 300000" results.csv charts

* Convert all svgs to pngs suitable for web:

cd charts
for f in *.svg; do rsvg-convert -z 2.0 $f -o ${f/.svg/.png}; done

* Convert all svgs to good quality pdfs:

cd charts
for f in *.svg; do inkscape -z -f $f --export-pdf=${f/.svg/.pdf}; done

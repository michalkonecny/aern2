* Regenerate results.csv:

rm results.csv
bash runBench.sh results.csv

* Generate svg execution time charts:

aern2-bench-chart FnOpReprs ExecTime results.csv charts

* Convert all svgs to pngs suitable for web:

cd charts
for f in *.svg; do rsvg-convert -z 2.0 $f -o ${f/.svg/.png}; done

* Convert all svgs to good quality pdfs:

cd charts
for f in *.svg; do inkscape -z -f $f --export-pdf=${f/.svg/.pdf}; done

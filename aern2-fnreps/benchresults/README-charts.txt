Convert all svgs to pngs suitable for web:

for f in *.svg; do rsvg-convert -z 2.0 $f -o ${f/.svg/.png}; done

Convert all svgs to good quality pdfs:

for f in *.svg; do inkscape -z -f $f --export-pdf=${f/.svg/.pdf}; done

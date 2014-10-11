SVGCSS= svg.css
SVG= *.svg
HTML= example.html

all: svg example_with_style.html

svg:
	../../src/updatesvg $(SVGCSS) $(SVG)

example_with_style.html: $(HTML)
	echo '<link href="style.css" rel="stylesheet" type="text/css">'  > $@
	cat $(HTML) >> $@

clean:
	rm -f *.svg *.html

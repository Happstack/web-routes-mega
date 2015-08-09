all: SimpleSite1.html  SimpleSite2.html  SimpleSite3.html \
    hscolour.css

%.html: %.lhs
	HsColour -css $^ -o$@ -lit

hscolour.css:
	HsColour -print-css > $@


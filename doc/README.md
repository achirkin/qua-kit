# Requirements

 * python-jinja2
 * python-bs4
 * latexmk
 * make
 * latex packages:
   * acronym
   * amsfonts
   * bytefield
   * enumitem
   * epstopdf
   * geometry
   * indentfirst
   * listings
   * msc
   * paralist
   * parskip
   * subcaption
   * url
   * wrapfig

# Compile on Ubuntu:

```
sudo apt-get install -y make python-jinja2 python-bs4 latexmk
sudo apt-get install -y texlive-latex-recommended texlive-latex-extra texlive-science
make
```

#!/bin/bash
if [[ -z "$1" ]]; then
    echo "Type the commit comment!" &1>2
    exit 1
fi
git pull https://github.com/char-lie/diploma
git add common/*.tex common/*.sty common/*.inc common/*.bib
git add README.md
git add practice/*.tex practice/index.pdf practice/*.latexmain practice/images/*.png
git add pull.sh
git commit -m "$1"
git push https://char-lie@github.com/char-lie/diploma

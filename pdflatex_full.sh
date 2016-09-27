#!/bin/sh

FILENAME="${1:-index}"
pdflatex ${FILENAME}.tex && bibtex ${FILENAME} && \
pdflatex ${FILENAME}.tex && pdflatex ${FILENAME}.tex

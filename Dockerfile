FROM harshjv/texlive-2015

RUN tlmgr update --all

RUN mkdir /tmp/shell-scripts
COPY install-pscyr.sh /tmp/shell-scripts/
RUN bash /tmp/shell-scripts/install-pscyr.sh
COPY pdflatex_full.sh /usr/bin/pdflatex_full


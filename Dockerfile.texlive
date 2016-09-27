FROM harshjv/texlive-2015

COPY install-pscyr.sh /tmp/shell-scripts/install-pscyr.sh
RUN bash /tmp/shell-scripts/install-pscyr.sh && \
    rm -rf /tmp/shell-scripts

COPY pdflatex_full.sh /usr/bin/pdflatex_full

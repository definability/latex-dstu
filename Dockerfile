FROM harshjv/texlive-2015

RUN tlmgr update --all

RUN mkdir /tmp/shell-scripts && \
    install-pscyr.sh /tmp/shell-scripts/ && \
    bash /tmp/shell-scripts/install-pscyr.sh && \
    rm -rf /tmp/shell-scripts

COPY pdflatex_full.sh /usr/bin/pdflatex_full

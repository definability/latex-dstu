FROM babbage/latex-dstu:texlive

RUN tlmgr update --all

COPY common /var/common


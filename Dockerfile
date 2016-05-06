FROM babbage/latex-dstu

RUN tlmgr update --all

COPY common /var/common


# LaTeX DSTU 3008-95
DSTU 3008-95 TeXLive template with Dockerfile to create thesis

## DSTU 3008-95 Template

Basic template was got from
http://dkhramov.dp.ua/Comp/NIRReportDSTU300895


## TeX Live 2015: PSCyr

Following instruction was used to install PSCyr to enable Times New Roman font
http://alice.saske.sk/wiki/TeXLive#PSCyr

Inserted in Docker with tag `texlive`:
```
docker pull babbage/latex-dstu:texlive
```

## Docker

Based on Docker package provided by @harshjv
https://github.com/harshjv/docker-texlive-2015

Link on Dockerhub
https://hub.docker.com/r/harshjv/texlive-2015

## Usage

Just go to folder with project and run
```
docker run --rm -it -v $(pwd):/var/texlive \
           babbage/latex-dstu pdflatex index.tex
```

To use your own `common` styles folder
```
docker run --rm -it -v $(pwd):/var/texlive -v $(pwd)/../common:/var/common \
           babbage/latex-dstu pdflatex index.tex
```


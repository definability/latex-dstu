# LaTeX DSTU 3008-95
DSTU 3008-95 TeXLive template with Dockerfile to create thesis

## DSTU 3008-95 Template

Basic template was got from
http://dkhramov.dp.ua/Comp/NIRReportDSTU300895


## TeX Live 2015: PSCyr

Following instruction was used to install PSCyr to enable Times New Roman font
http://alice.saske.sk/wiki/TeXLive#PSCyr

Inserted in Docker with tag `texlive`:
```bash
docker pull babbage/latex-dstu:texlive
```

## Docker

Based on Docker package provided by @harshjv
https://github.com/harshjv/docker-texlive-2015

Link on Dockerhub
https://hub.docker.com/r/harshjv/texlive-2015

Link to this image on Dockerhub
https://hub.docker.com/r/babbage/latex-dstu/

## Usage

Just go to folder with project and run
```bash
docker run --rm -it -v $(pwd):/var/texlive babbage/latex-dstu pdflatex_full
```

To use your own `common` styles folder
```bash
docker run --rm -it -v $(pwd):/var/texlive -v $(pwd)/../common:/var/common babbage/latex-dstu pdflatex_full index
```

### Notes

You have to run `pdflatex` multiple times to
- generate correct bibliography citations,
- generate table of contents.

It's recommended to delete temporary files before compilation
or when strange error occur.
Assuming that your index file called `index.tex`:
```bash
sudo rm -f index.{bbl,blg,log,toc,aux,out,ist,glo,dat}
```

Parameter `index` for `pdflatex_full` is optional.
You should specify your index file name. If it's `index.tex`,
you can use `pdflatex_full index` either `pdflatex_full`.

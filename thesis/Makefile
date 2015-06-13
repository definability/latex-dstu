R_INTERPRETER=R_ENABLE_JIT=3 Rscript

pdf:
	pdflatex index.tex

move_images:
	mv ./code/*.png ./images/

move_texfiles:
	mv ./code/*.tex ./6_practice/

move_all:
	make move_images
	make move_texfiles

types:
	cd ./code; \
		$(R_INTERPRETER) types.r

exam:
	cd ./code; \
		$(R_INTERPRETER) exam.r

build:
	pandoc --pdf-engine=xelatex --biblatex -o out.pdf $(WORK_FILE)

tex:
	pandoc -o out.tex $(WORK_FILE)

watch:
	watchexec 'make build' --watch $(WORK_FILE)

watch-tex:
	watchexec 'make tex' --watch $(WORK_FILE)

open:
	xdg-open out.pdf

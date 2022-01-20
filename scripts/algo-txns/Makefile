all: render-TestNet.png render-MainNet.png

render-%.png: data-%.csv render.gpi
	gnuplot -e "filename='$@'" -e "datafile='$<'" -e "algoNet='$*'" render.gpi

# https://app.flipsidecrypto.com/velocity/queries/039c232b-44aa-4d44-b707-cf07d1d1c75b
data-%.csv: flipside.py flipside-%.csv
	./$^ > $@

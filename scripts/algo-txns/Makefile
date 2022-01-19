all: render-TestNet.png

render-%.png: data-%.csv render.gpi
	gnuplot -e "filename='$@'" -e "datafile='$<'" -e "algoNet='$*'" render.gpi

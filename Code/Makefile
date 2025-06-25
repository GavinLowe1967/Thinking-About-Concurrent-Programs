
ROOT = tacp

all:
	cd Util; make
	cd 01.Intro; make
	cd 02.Channels; make
	cd 03.Trapezium; make
	cd 04.Alternation; make
	cd 05.ClientServer; make

clean:
	rm $(ROOT)/*/*.class; fsc -shutdown

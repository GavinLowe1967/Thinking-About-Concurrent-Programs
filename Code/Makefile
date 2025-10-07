
ROOT = tacp

all:
	cd Util; make
	cd 01.Intro; make
	cd 02.Channels; make
	cd 03.Trapezium; make
	cd 04.Alternation; make
	cd 05.ClientServer; make
	cd 06.InteractingPeers; make
	cd 07.ConcurrentDatatypes; make
	cd 08.DataParallel; make
	cd 09.Locks; make
	cd 10.Conditions; make

clean:
	rm $(ROOT)/*/*.class; fsc -shutdown

ROOT = ../tacp

DIR = $(ROOT)/dataParallel

DTDIR = $(ROOT)/datatypes

DT_SRC_DIR = ../07.ConcurrentDatatypes

all: $(DIR)/BarrierTest.class $(DIR)/PrefixSums.class $(DIR)/JacobiTest.class $(DIR)/JacobiExperiment.class $(DIR)/BFSGraphSearch.class $(DIR)/Gravity.class

$(DIR)/BarrierTest.class: $(DIR)/ServerBarrier.class

$(DIR)/JacobiTest.class $(DIR)/JacobiExperiment.class: $(DIR)/Jacobi.class

$(DIR)/BFSGraphSearch.class: $(DTDIR)/GraphSearch.class $(DTDIR)/ConcSet.class $(DTDIR)/TotalQueue.class

clean:
	rm $(DIR)/*.class; fsc -shutdown

# CP = ..:/home/gavinl/Scala/Uti

FSC = fsc -deprecation -cp .. -d ..

$(DIR)/%.class:     %.scala
	$(FSC)  $<

$(DTDIR)/%.class: $(DT_SRC_DIR)/%.scala
	cd $(DT_SRC_DIR); make

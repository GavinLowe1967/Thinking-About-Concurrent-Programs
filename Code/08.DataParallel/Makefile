ROOT = ../tacp

DIR = $(ROOT)/dataParallel

DTDIR = $(ROOT)/datatypes

DT_SRC_DIR = ../07.ConcurrentDatatypes

EXERCISES = $(DIR)/ServerCombiningBarrier.class $(DIR)/BarrierLog.class	\
  $(DIR)/PrefixSumsShared.class $(DIR)/ArrayMax.class			\
  $(DIR)/SmoothSharedTest.class $(DIR)/LargestComponent.class

all: $(DIR)/BarrierTest.class $(DIR)/PrefixSumsTest.class	\
  $(DIR)/JacobiTest.class $(DIR)/JacobiExperiment.class		\
  $(DIR)/BFSGraphSearch.class $(DIR)/Gravity.class $(EXERCISES)

$(DIR)/BarrierTest.class: $(DIR)/ServerBarrier.class $(DIR)/BarrierLog.class

$(DIR)/JacobiTest.class $(DIR)/JacobiExperiment.class: $(DIR)/Jacobi.class

$(DIR)/BFSGraphSearch.class: $(DTDIR)/GraphSearch.class $(DTDIR)/ConcSet.class $(DTDIR)/TotalQueue.class

$(DIR)/ServerCombiningBarrier.class: $(DIR)/BarrierLog.class

$(DIR)/PrefixSumsSgared.class: $(DIR)/PrefixSums.class

$(DIR)/PrefixSumsTest.class: $(DIR)/PrefixSumsShared.class

$(DIR)/SmoothSharedTest.class: $(DIR)/SmoothShared.class

$(DIR)/SmoothShared.class: $(DIR)/Smooth.class

clean:
	rm $(DIR)/*.class; fsc -shutdown

# CP = ..:/home/gavinl/Scala/Uti

FSC = fsc -deprecation -cp .. -d ..

$(DIR)/%.class:     %.scala
	$(FSC)  $<

$(DTDIR)/%.class: $(DT_SRC_DIR)/%.scala
	cd $(DT_SRC_DIR); make

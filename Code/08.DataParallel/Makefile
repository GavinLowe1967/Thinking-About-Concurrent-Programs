ROOT = ../tacp

DIR = $(ROOT)/dataParallel
DTDIR = $(ROOT)/datatypes
MONDIR = $(ROOT)/monitors

DT_SRC_DIR = ../07.ConcurrentDatatypes
MON_SRC_DIR = ../10.Conditions

EXERCISES = $(DIR)/ServerCombiningBarrier.class $(DIR)/BarrierLog.class	\
  $(DIR)/PrefixSumsShared.class $(DIR)/ArrayMax.class			\
  $(DIR)/SmoothSharedTest.class $(DIR)/LargestComponent.class

all: $(DIR)/BarrierTest.class $(DIR)/PrefixSumsTest.class	\
  $(DIR)/JacobiTest.class $(DIR)/JacobiExperiment.class		\
  $(DIR)/BFSGraphSearch.class $(DIR)/Gravity.class $(EXERCISES)

$(DIR)/BarrierTest.class: $(DIR)/ServerBarrier.class $(DIR)/BarrierLog.class $(MONDIR)/ConditionsBarrier.class

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

$(MONDIR)/%.class: $(MON_SRC_DIR)/%.scala
	cd $(MON_SRC_DIR); make

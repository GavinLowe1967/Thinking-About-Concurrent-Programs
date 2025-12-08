ROOT = ../tacp

DIR = $(ROOT)/dataParallel
DTDIR = $(ROOT)/datatypes
MONDIR = $(ROOT)/monitors
JVM_MON_DIR = $(ROOT)/jvmMonitors

DT_SRC_DIR = ../07.ConcurrentDatatypes
MON_SRC_DIR = ../10.Conditions
JVM_MON_SRC_DIR = ../11.JVMMonitors

EXERCISES = $(DIR)/ServerCombiningBarrier.class $(DIR)/BarrierLog.class	\
  $(DIR)/PrefixSumsShared.class $(DIR)/ArrayMax.class			\
  $(DIR)/SmoothSharedTest.class $(DIR)/LargestComponent.class

all: $(DIR)/PrefixSumsTest.class $(DIR)/JacobiTest.class	\
  $(DIR)/JacobiExperiment.class $(DIR)/BFSGraphSearch.class	\
  $(DIR)/Gravity.class $(EXERCISES) $(DIR)/CombiningBarrierTest.class

# $(DIR)/BarrierTest.class

# $(DIR)/BarrierTest.class: $(DIR)/ServerBarrier.class $(DIR)/BarrierLog.class $(MONDIR)/ConditionsBarrier.class $(JVM_MON_DIR)/JVMMonitorBarrier.class

$(DIR)/JacobiTest.class $(DIR)/JacobiExperiment.class: $(DIR)/Jacobi.class

$(DIR)/BFSGraphSearch.class: $(DTDIR)/GraphSearch.class $(DTDIR)/ConcSet.class $(DTDIR)/TotalQueue.class

# Barriers

$(DIR)/CombiningBarrierTest.class: $(DIR)/ServerCombiningBarrier.class $(DIR)/CombiningBarrierLog.class

$(DIR)/BarrierLog.class: $(DIR)/ServerCombiningBarrier.class $(DIR)/ServerBarrier.class

barrier: $(DIR)/BarrierLog.class

# Prefix sums

$(DIR)/PrefixSumsShared.class: $(DIR)/PrefixSums.class

$(DIR)/PrefixSumsTest.class: $(DIR)/PrefixSumsShared.class

# Smoothing

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

$(JVM_MON_DIR)/JVMMonitorBarrier.class: $(JVM_MON_SRC_DIR)/JVMMonitorBarrier.scala
	cd $(JVM_MON_SRC_DIR); make barrier

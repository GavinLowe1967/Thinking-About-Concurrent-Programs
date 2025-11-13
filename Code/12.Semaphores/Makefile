ROOT = ../tacp

# Directories for objects

DIR = $(ROOT)/semaphores
DPDIR = $(ROOT)/dataParallel
DTDIR = $(ROOT)/datatypes
MONDIR = $(ROOT)/monitors
JVM_MON_DIR = $(ROOT)/jvmMonitors

# Source directories for code built on

DT_SRC_DIR = ../07.ConcurrentDatatypes
DP_SRC_DIR = ../08.DataParallel
MON_SRC_DIR = ../10.Conditions
JVM_MON_SRC_DIR = ../11.JVMMonitors

all: $(DIR)/Counter.class $(DTDIR)/SemaphorePartialQueue.class $(DIR)/SemaphoreBarrier.class $(DIR)/SemaphoreSyncChan.class $(DIR)/QueueLock.class

# Partial queues

$(DTDIR)/SemaphorePartialQueue.class: $(DTDIR)/PartialQueue.class

$(DTDIR)/PartialQueue.class: $(DT_SRC_DIR)/PartialQueue.scala
	cd $(DT_SRC_DIR); make pQueue

# Barrier synchronisation

$(DIR)/SemaphoreBarrier.class: $(DPDIR)/ServerBarrier.class

$(DPDIR)/ServerBarrier.class: $(DP_SRC_DIR)/ServerBarrier.scala
	cd $(DP_SRC_DIR); make barrier

barrier: $(DIR)/SemaphoreBarrier.class

# Synchronous channel

$(DIR)/SemaphoreSyncChan.class: $(MONDIR)/SyncChanT.class

$(MONDIR)/SyncChanT.class: $(MON_SRC_DIR)/SyncChanT.scala
	cd $(MON_SRC_DIR); make syncChan

syncChan: $(DIR)/SemaphoreSyncChan.class

# Lock

$(DIR)/QueueLock.class: $(JVM_MON_DIR)/Lock.class

$(JVM_MON_DIR)/Lock.class: $(JVM_MON_SRC_DIR)/Lock.scala
	cd $(JVM_MON_SRC_DUR); make lock

lock: $(DIR)/QueueLock.class

# Standard recipes

clean:
	rm $(DIR)/*.class; fsc -shutdown

FSC = fsc -d .. -deprecation -cp ..

$(DIR)/%.class $(DTDIR)/%.class:     %.scala
	$(FSC)  $<

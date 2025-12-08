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

EXAMPLES = $(DIR)/Counter.class $(DTDIR)/SemaphorePartialQueue.class	\
  $(DIR)/SemaphoreBarrier.class $(DIR)/SemaphoreSyncChan.class		\
  $(DIR)/QueueLock.class $(DIR)/SemaphoreReadersWriters.class

EXERCISES = $(DIR)/SemaphoreReadersWritersQueueLock.class	\
  $(DIR)/SemaphoreProducerConsumer.class $(DIR)/Mod3S.class	\
  $(DTDIR)/BoundedSemaphorePartialQueue.class $(DIR)/MenWomenSemaphore.class

all: $(EXAMPLES) $(EXERCISES)

# Partial queues

$(DTDIR)/SemaphorePartialQueue.class $(DTDIR)/BoundedSemaphorePartialQueue.class: $(DTDIR)/PartialQueue.class

$(DTDIR)/PartialQueue.class: $(DT_SRC_DIR)/PartialQueue.scala
	cd $(DT_SRC_DIR); make pQueue

pQueue: $(DTDIR)/SemaphorePartialQueue.class

bpQueue:  $(DTDIR)/BoundedSemaphorePartialQueue.class

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
	cd $(JVM_MON_SRC_DIR); make lock

lock: $(DIR)/QueueLock.class

# Readers and writers

$(DIR)/SemaphoreReadersWriters.class: $(MONDIR)/ReadersWritersLock.class

$(MONDIR)/ReadersWritersLock.class: $(MON_SRC_DIR)/ReadersWritersLock.scala
	cd $(MON_SRC_DIR); make readersWriters

readersWriters: $(DIR)/SemaphoreReadersWriters.class

# Producer consumer

producerConsumer: $(DIR)/SemaphoreProducerConsumer.class

# Mod 3

mod3: $(DIR)/Mod3S.class

# Men and women

menWomen: $(DIR)/MenWomenSemaphore.class

# Standard recipes

clean:
	rm $(DIR)/*.class; fsc -shutdown

FSC = fsc -d .. -deprecation -cp ..

$(DIR)/%.class $(DTDIR)/%.class:     %.scala
	$(FSC)  $<

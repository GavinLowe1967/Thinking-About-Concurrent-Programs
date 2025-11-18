ROOT = ../tacp

DIR = $(ROOT)/tests
DPDIR = $(ROOT)/dataParallel
DTDIR = $(ROOT)/datatypes
MONDIR = $(ROOT)/monitors
JVM_MON_DIR = $(ROOT)/jvmMonitors
SEMDIR = $(ROOT)/semaphores

# Source directories for classes that use tests here.

DT_SRC_DIR = ../07.ConcurrentDatatypes
DP_SRC_DIR = ../08.DataParallel
COND_SRC_DIR = ../10.Conditions
JVM_MON_SRC_DIR = ../11.JVMMonitors
SEM_SRC_DIR = ../12.Semaphores

all: $(DIR)/PartialQueueTest.class $(DIR)/BarrierTest.class	\
  $(DIR)/SyncChanTest.class $(DIR)/LockTest.class		\
  $(DIR)/ReadersWritersTest.class

# Partial queues

$(DIR)/PartialQueueTest.class: $(DTDIR)/PartialQueue.class			\
  $(DTDIR)/MonitorPartialQueue.class $(DTDIR)/JVMMonitorPartialQueue.class	\
  $(DTDIR)/SemaphorePartialQueue.class	

$(DTDIR)/PartialQueue.class: $(DT_SRC_DIR)/PartialQueue.scala
	cd $(DT_SRC_DIR); make pQueue

$(DTDIR)/MonitorPartialQueue.class: $(COND_SRC_DIR)/MonitorPartialQueue.scala
	cd $(COND_SRC_DIR); make queue

$(DTDIR)/JVMMonitorPartialQueue.class: $(JVM_MON_SRC_DIR)/JVMMonitorPartialQueue.scala
	cd $(JVM_MON_SRC_DIR); make pQueue

$(DTDIR)/SemaphorePartialQueue.class: $(SEM_SRC_DIR)/SemaphorePartialQueue.scala
	cd $(SEM_SRC_DIR); make pQueue

# Barrier synchronisation objects

$(DIR)/BarrierTest.class: $(DPDIR)/ServerBarrier.class				\
  $(DPDIR)/ServerCombiningBarrier.class $(DPDIR)/BarrierLog.class		\
  $(MONDIR)/ConditionsBarrier.class $(JVM_MON_DIR)/JVMMonitorBarrier.class	\
  $(SEMDIR)/SemaphoreBarrier.class

$(DPDIR)/ServerBarrier.class: $(DP_SRC_DIR)/ServerBarrier.scala
	cd $(DP_SRC_DIR); make barrier

$(DPDIR)/ServerCombiningBarrier.class: $(DP_SRC_DIR)/ServerCombiningBarrier.scala
	cd $(DP_SRC_DIR); make barrier

$(DPDIR)/BarrierLog.class: $(DP_SRC_DIR)/BarrierLog.scala
	cd $(DP_SRC_DIR); make barrier

$(MONDIR)/ConditionsBarrier.class: $(COND_SRC_DIR)/ConditionsBarrier.scala
	cd $(COND_SRC_DIR); make barrier

$(JVM_MON_DIR)/JVMMonitorBarrier.class: $(JVM_MON_SRC_DIR)/JVMMonitorBarrier.scala
	cd $(JVM_MON_SRC_DIR); make barrier

$(SEMDIR)/SemaphoreBarrier.class: $(SEM_SRC_DIR)/SemaphoreBarrier.scala
	cd $(SEM_SRC_DIR); make barrier

# Synchronous channel

$(DIR)/SyncChanTest.class: $(MONDIR)/MonitorSyncChan.class $(JVM_MON_DIR)/JVMMonitorSyncChan.class $(SEMDIR)/SemaphoreSyncChan.class

$(DIR)/MonitorSyncChan.class: $(COND_SRC_DIR)/MonitorSyncChan.scala
	cd $(COND_SRC_DIR); make syncChan

$(JVM_MON_DIR)/JVMMonitorSyncChan.class: $(JVM_MON_SRC_DIR)/JVMMonitorSyncChan.scala
	cd $(JVM_MON_SRC_DIR); make syncChan

$(SEMDIR)/SemaphoreSyncChan.class: $(SEM_SRC_DIR)/SemaphoreSyncChan.scala
	cd $(SEM_SRC_DIR); make syncChan

# Lock

$(DIR)/LockTest.class: $(JVM_MON_DIR)/Lock.class $(SEMDIR)/QueueLock.class

$(JVM_MON_DIR)/Lock.class: $(JVM_MON_SRC_DIR)/Lock.scala
	cd $(JVM_MON_SRC_DIR); make lock

$(SEMDIR)/QueueLock.class: $(SEM_SRC_DIR)/QueueLock.scala
	cd $(SEM_SRC_DIR); make lock

# Readers and writers

$(DIR)/ReadersWritersTest.class: $(MONDIR)/MonitorReadersWriters.class $(SEMDIR)/SemaphoreReadersWriters.class

$(MONDIR)/MonitorReadersWriters.class: $(COND_SRC_DIR)/MonitorReadersWriters.scala $(COND_SRC_DIR)/ReadersWritersLock.scala
	cd $(COND_SRC_DIR); make readersWriters

$(SEMDIR)/SemaphoreReadersWriters.class: $(SEM_SRC_DIR)/SemaphoreReadersWriters.scala
	cd $(SEM_SRC_DIR); make readersWriters

# Standard recipes

clean:
	rm $(DIR)/*.class; fsc -shutdown

FSC = fsc -deprecation -cp .. -d ..

$(DIR)/%.class:     %.scala
	$(FSC)  $<

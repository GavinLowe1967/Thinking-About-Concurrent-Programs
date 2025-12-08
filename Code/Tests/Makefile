ROOT = ../tacp

DIR = $(ROOT)/tests
CSDIR = $(ROOT)/clientServer
DPDIR = $(ROOT)/dataParallel
DTDIR = $(ROOT)/datatypes
MONDIR = $(ROOT)/monitors
JVM_MON_DIR = $(ROOT)/jvmMonitors
SEMDIR = $(ROOT)/semaphores

# Source directories for classes that use tests here.

CS_SRC_DIR = ../05.ClientServer
DT_SRC_DIR = ../07.ConcurrentDatatypes
DP_SRC_DIR = ../08.DataParallel
LOCK_SRC_DIR = ../09.Locks
COND_SRC_DIR = ../10.Conditions
JVM_MON_SRC_DIR = ../11.JVMMonitors
SEM_SRC_DIR = ../12.Semaphores

DATATYPES = $(DIR)/PartialQueueTest.class $(DIR)/MapTest.class		\
  $(DIR)/PartialStackTest.class $(DIR)/BoundedPartialStackTest.class	\
  $(DIR)/TotalStackTest.class $(DIR)/BoundedPartialQueueTest.class

LOCKS = $(DIR)/LockTest.class $(DIR)/ReadersWritersTest.class	\
  $(DIR)/DisjointLockTest.class $(DIR)/Mod3Test.class

all: $(DATATYPES) $(LOCKS) $(DIR)/BarrierTest.class $(DIR)/SyncChanTest.class	\
  $(DIR)/ProducerConsumerTest.class $(DIR)/MenWomenTest.class

######### Concurrent datatypes

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

# Bounded partial queues

$(DIR)/BoundedPartialQueueTest.class:  $(DTDIR)/BoundedMonitorPartialQueue.class $(DTDIR)/JVMMonitorBoundedPartialQueue.class $(DTDIR)/BoundedSemaphorePartialQueue.class

$(DTDIR)/BoundedMonitorPartialQueue.class: $(COND_SRC_DIR)/BoundedMonitorPartialQueue.scala
	cd $(COND_SRC_DIR); make bpQueue

$(DTDIR)/JVMMonitorBoundedPartialQueue.class: $(JVM_MON_SRC_DIR)/JVMMonitorBoundedPartialQueue.scala
	cd $(JVM_MON_SRC_DIR); make bpQueue

$(DTDIR)/BoundedSemaphorePartialQueue.class: $(SEM_SRC_DIR)/BoundedSemaphorePartialQueue.scala
	cd $(SEM_SRC_DIR); make bpQueue

# Partial stacks

$(DIR)/PartialStackTest.class: $(DIR)/ImmutableStack.class $(DTDIR)/PartialStack.class

$(DTDIR)/PartialStack.class: $(DT_SRC_DIR)/PartialStack.scala
	cd $(DT_SRC_DIR); make pStack

# Bounded partial stacks

$(DIR)/BoundedPartialStackTest.class: $(DIR)/ImmutableStack.class $(DTDIR)/PartialStack.class $(DTDIR)/MonitorBoundedPartialStack.class $(DTDIR)/JVMMonitorBoundedPartialStack.class

$(DTDIR)/MonitorBoundedPartialStack.class: $(COND_SRC_DIR)/MonitorBoundedPartialStack.scala
	cd $(COND_SRC_DIR); make pStack

$(DTDIR)/JVMMonitorBoundedPartialStack.class: $(JVM_MON_SRC_DIR)/JVMMonitorBoundedPartialStack.scala
	cd $(JVM_MON_SRC_DIR); make

# Total stacks

$(DIR)/TotalStackTest.class: $(DIR)/ImmutableStack.class $(DTDIR)/TotalStack.class

$(DTDIR)/TotalStack.class: $(DT_SRC_DIR)/TotalStack.scala
	cd $(DT_SRC_DIR); make tStack


# Maps

$(DIR)/MapTest.class: $(DTDIR)/ShardedMap.class $(DTDIR)/LinkedListMap.class  $(DTDIR)/JVMMonitorShardedMap.class

$(DTDIR)/ShardedMap.class: $(LOCK_SRC_DIR)/ShardedMap.scala
	cd $(LOCK_SRC_DIR); make maps

$(DTDIR)/LinkedListMap.class:  $(LOCK_SRC_DIR)/LinkedListMap.scala
	cd $(LOCK_SRC_DIR); make maps

$(DTDIR)/JVMMonitorShardedMap.class: $(JVM_MON_SRC_DIR)/JVMMonitorShardedMap.scala
	cd $(JVM_MON_SRC_DIR); make maps

# Producer-consumer buffer example

$(DIR)/ProducerConsumerTest.class: $(MONDIR)/MonitorProducerConsumer.class $(SEMDIR)/SemaphoreProducerConsumer.class

$(MONDIR)/MonitorProducerConsumer.class: $(COND_SRC_DIR)/MonitorProducerConsumer.scala
	cd $(COND_SRC_DIR); make producerConsumer

$(SEMDIR)/SemaphoreProducerConsumer.class: $(SEM_SRC_DIR)/SemaphoreProducerConsumer.scala
	cd $(SEM_SRC_DIR); make producerConsumer

######### Synchronisation objects

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

######### Locks
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

# Disjoint Lock

$(DIR)/DisjointLockTest.class: $(MONDIR)/MonitorDisjointLock.class

$(MONDIR)/MonitorDisjointLock.class: $(COND_SRC_DIR)/MonitorDisjointLock.scala
	cd $(COND_SRC_DIR); make disjointLock

# Mod3

$(DIR)/Mod3Test.class: $(JVM_MON_DIR)/Mod3M.class $(SEMDIR)/Mod3S.class

$(JVM_MON_DIR)/Mod3M.class: $(JVM_MON_SRC_DIR)/Mod3M.scala
	cd $(JVM_MON_SRC_DIR); make mod3

$(SEM_DIR)/Mod3S.class: $(SEM_SRC_DIR)/Mod3S.scala
	cd $(SEM_SRC_DIR)/; make mod3

######### Standalone programs

# Men and women

$(DIR)/MenWomenTest.class: $(CSDIR)/MenWomen.class $(MONDIR)/MenWomenM.class $(SEMDIR)/MenWomenSemaphore.class

$(CSDIR)/MenWomen.class: $(CS_SRC_DIR)/MenWomen.scala
	cd $(CS_SRC_DIR); make menWomen

$(MONDIR)/MenWomenM.class: $(COND_SRC_DIR)/MenWomenM.scala
	cd $(COND_SRC_DIR); make menWomen

$(SEMDIR)/MenWomenSemaphore.class: $(SEM_SRC_DIR)/MenWomenSemaphore.scala
	cd $(SEM_SRC_DIR); make menWomen

########## Standard recipes

clean:
	rm $(DIR)/*.class; fsc -shutdown

FSC = fsc -deprecation -cp .. -d ..

$(DIR)/%.class:     %.scala
	$(FSC)  $<

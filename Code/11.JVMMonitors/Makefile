ROOT = ../tacp

# Directories for objects

DIR = $(ROOT)/jvmMonitors
DTDIR = $(ROOT)/datatypes
MONDIR = $(ROOT)/monitors
DPDIR = $(ROOT)/dataParallel

# Directories for source code.

DP_SRC_DIR = ../08.DataParallel
LOCK_SRC_DIR = ../09.Locks
DT_SRC_DIR = ../07.ConcurrentDatatypes
MON_SRC_DIR = ../10.Conditions

QUEUES = $(DTDIR)/MonitorTotalQueue.class	\
   $(DTDIR)/JVMMonitorPartialQueue.class	\
   $(DTDIR)/JVMMonitorBoundedPartialQueue.class

EXERCISES = $(DIR)/Mod3M.class $(DTDIR)/JVMMonitorShardedMap.class	\
  $(DIR)/HeapBarrier.class $(DTDIR)/JVMMonitorBoundedPartialStack.class

all: $(QUEUES) $(DIR)/Counter.class $(DTDIR)/MonitorShardedSet.class	\
  $(DIR)/JVMMonitorSyncChan.class $(DIR)/Lock.class			\
  $(DIR)/JVMMonitorBarrier.class $(DIR)/TimeoutExchanger.class $(EXERCISES)

######### Straightforward examples


striped: $(DIR)/StripedCounterArray.class

######### Concurrent datatypes

shardedSet: $(DTDIR)/MonitorShardedSet.class

$(DTDIR)/MonitorShardedSet.class $(DTDIR)/JVMMonitorShardedMap.class: $(DTDIR)/Sharding.class

$(DTDIR)/Sharding.class: $(LOCK_SRC_DIR)/Sharding.scala
	cd $(LOCK_SRC_DIR); make sharding

# Total queue

$(DTDIR)/MonitorTotalQueue.class: $(DTDIR)/TotalQueue.class

$(DTDIR)/TotalQueue.class: $(DT_SRC_DIR)/TotalQueue.scala
	cd $(DT_SRC_DIR); make tQueue

tQueue:  $(DTDIR)/MonitorTotalQueue.class

# Partial queue

$(DTDIR)/JVMMonitorPartialQueue.class $(DTDIR)/JVMMonitorBoundedPartialQueue.class: $(DTDIR)/PartialQueue.class

$(DTDIR)/PartialQueue.class: $(DT_SRC_DIR)/PartialQueue.scala
	cd $(DT_SRC_DIR); make pQueue

pQueue: $(DTDIR)/JVMMonitorPartialQueue.class

bpQueue: $(DTDIR)/JVMMonitorBoundedPartialQueue.class

# Bounded partial stack

bpStack:  $(DTDIR)/JVMMonitorBoundedPartialStack.class

# Maps

$(DTDIR)/JVMMonitorShardedMap.class:  $(DTDIR)/Map.class

$(DTDIR)/Map.class: $(LOCK_SRC_DIR)/Map.scala
	cd $(LOCK_SRC_DIR); make maps

maps: $(DTDIR)/JVMMonitorShardedMap.class



######### Synchronisation objects
# Synchronous channel

$(DIR)/JVMMonitorSyncChan.class: $(MONDIR)/SyncChanT.class

$(MONDIR)/SyncChanT.class: $(MON_SRC_DIR)/SyncChanT.scala
	cd $(MON_SRC_DIR); make syncChan

syncChan: $(DIR)/JVMMonitorSyncChan.class

# Barrier

$(DIR)/JVMMonitorBarrier.class: $(DPDIR)/ServerBarrier.class

$(DPDIR)/ServerBarrier.class: $(DP_SRC_DIR)/ServerBarrier.scala
	cd $(DP_SRC_DIR); make

barrier: $(DIR)/JVMMonitorBarrier.class

######### Locks
# Lock

lock: $(DIR)/Lock.class

# Mod3

mod3: $(DIR)/Mod3M.class

########## Standard recipes

clean:
	rm $(DIR)/*.class; fsc -shutdown

FSC = fsc -d .. -deprecation -cp ..

$(DIR)/%.class $(DTDIR)/%.class:     %.scala
	$(FSC)  $<

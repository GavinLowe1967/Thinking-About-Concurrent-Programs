ROOT = ../tacp

DIR = $(ROOT)/locks
DTDIR = $(ROOT)/datatypes
TRAPDIR = $(ROOT)/trapezium
MONDIR = $(ROOT)/jvmMonitors

TRAP_SRC_DIR = ../03.Trapezium
MON_SRC_DIR = ../11.JVMMonitors

EXERCISES = $(DTDIR)/LinkedListMap.class  $(DTDIR)/ShardedMap.class $(DTDIR)/ConcurrentIterator.class $(DTDIR)/QueueArray.class 

# $(DTDIR)/TerminatingQueueArray.class

all: $(DIR)/Counter.class $(DTDIR)/LockTotalQueue.class $(TRAPDIR)/BagOfTasksLock.class $(DIR)/CounterArrayTest.class  $(DTDIR)/SetTest.class $(EXERCISES)

# Counter array

$(DIR)/CounterArrayTest.class: $(DIR)/CounterArray.class $(MONDIR)/StripedCounterArray.class

$(MONDIR)/StripedCounterArray.class: $(MON_SRC_DIR)/StripedCounterArray.scala
	cd $(MON_SRC_DIR); make striped

# Trapezium

$(TRAPDIR)/BagOfTasksLock.class: $(TRAPDIR)/BagOfTasks.class

$(TRAPDIR)/BagOfTasks.class: $(TRAP_SRC_DIR)/BagOfTasks.scala
	cd $(TRAP_SRC_DIR); make bagOfTasks

trapezium:  $(TRAPDIR)/BagOfTasksLock.class

######### Datatypes
# Sharding

$(DTDIR)/ShardedMap.class $(DTDIR)/ShardedSet.class: $(DTDIR)/Sharding.class

sharding: $(DTDIR)/Sharding.class

# Sets

$(DTDIR)/LinkedListSet.class: $(DTDIR)/ShardedSet.class

$(DTDIR)/SetTest.class: $(DTDIR)/ShardedSet.class $(DTDIR)/LinkedListSet.class $(DTDIR)/MonitorShardedSet.class

$(DTDIR)/MonitorShardedSet.class: $(MON_SRC_DIR)/MonitorShardedSet.scala
	cd $(MON_SRC_DIR); make shardedSet

# Maps

$(DTDIR)/ShardedMap.class $(DTDIR)/LinkedListMap.class: $(DTDIR)/Map.class

# $(DTDIR)/MapTest.class: $(DTDIR)/ShardedMap.class $(DTDIR)/LinkedListMap.class

maps: $(DTDIR)/ShardedMap.class $(DTDIR)/LinkedListMap.class

# QueueArray example

$(DTDIR)/QueueArray.class $(DTDIR)/TerminatingQueueArray.class: $(DIR)/LocalLinearizabilityTester.class

######### Standard recipes

clean:
	rm $(DIR)/*.class; fsc -shutdown

FSC = fsc -deprecation -cp .. -d ..

$(DIR)/%.class $(DTDIR)/%.class $(TRAPDIR)/%.class:     %.scala
	$(FSC)  $<

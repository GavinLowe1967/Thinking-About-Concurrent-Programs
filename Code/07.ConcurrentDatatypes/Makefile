ROOT = ../tacp

DIR = $(ROOT)/datatypes

DPDIR = $(ROOT)/dataParallel

# Source directories for classes that use tests here.

DP_SRC_DIR = ../08.DataParallel
LOCK_SRC_DIR = ../09.Locks
COND_SRC_DIR = ../10.Conditions
JVM_MON_SRC_DIR = ../11.JVMMonitors

EXERCISES = $(DIR)/TotalStack.class $(DIR)/Sudoku.class $(DIR)/PartialStack.class

all: $(DIR)/QueueTest.class $(DIR)/PartialQueue.class $(DIR)/WordPath.class $(EXERCISES)

clean:
	rm $(DIR)/*.class; fsc -shutdown

# Total queues

$(DIR)/QueueTest.class: $(DIR)/TotalQueue.class $(DIR)/LockTotalQueue.class $(DIR)/MonitorTotalQueue.class

$(DIR)/LockTotalQueue.class: $(LOCK_SRC_DIR)/LockTotalQueue.scala
	cd $(LOCK_SRC_DIR); make

$(DIR)/MonitorTotalQueue.class: $(JVM_MON_SRC_DIR)/MonitorTotalQueue.scala
	cd $(JVM_MON_SRC_DIR); make tQueue

tQueue: $(DIR)/TotalQueue.class

# Partial queues


pQueue: $(DIR)/PartialQueue.class

# BF search

$(DIR)/ConcBFGraphSearch.class: $(DIR)/GraphSearch.class $(DIR)/TerminatingPartialQueue.class $(DIR)/ConcSet.class

$(DIR)/WordPath.class:  $(DIR)/ConcBFGraphSearch.class $(DPDIR)/BFSGraphSearch.class

$(DPDIR)/BFSGraphSearch.class: $(DP_SRC_DIR)/BFSGraphSearch.scala
	cd $(DP_SRC_DIR); make

# Exercises

pStack: $(DIR)/PartialStack.class

tStack: $(DIR)/TotalStack.class


# DF search

$(DIR)/DFGraphSearch.class: $(DIR)/GraphSearch.class

$(DIR)/ConcDFGraphSearch.class: $(DIR)/DFGraphSearch.class $(DIR)/TerminatingPartialStack.class

$(DIR)/Sudoku.class: $(DIR)/ConcDFGraphSearch.class

FSC = fsc -deprecation -cp .. -d ..

$(DIR)/%.class:     %.scala
	$(FSC)  $<

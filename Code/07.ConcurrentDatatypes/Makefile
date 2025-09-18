ROOT = ../tacp

DIR = $(ROOT)/datatypes

DPDIR = $(ROOT)/dataParallel

DP_SRC_DIR = ../08.DataParallel

LOCK_SRC_DIR = ../09.Locks

EXERCISES = $(DIR)/TotalStack.class $(DIR)/Sudoku.class $(DIR)/PartialStack.class

all: $(DIR)/QueueTest.class $(DIR)/PartialQueueTest.class $(DIR)/WordPath.class $(EXERCISES)

clean:
	rm $(DIR)/*.class; fsc -shutdown

$(DIR)/QueueTest.class: $(DIR)/TotalQueue.class $(DIR)/LockTotalQueue.class

$(DIR)/PartialQueueTest.class: $(DIR)/PartialQueue.class

$(DIR)/LockTotalQueue.class: $(LOCK_SRC_DIR)/LockTotalQueue.scala
	cd $(LOCK_SRC_DIR); make

# BF search

$(DIR)/ConcBFGraphSearch.class: $(DIR)/GraphSearch.class $(DIR)/TerminatingPartialQueue.class $(DIR)/ConcSet.class

$(DIR)/WordPath.class:  $(DIR)/ConcBFGraphSearch.class $(DPDIR)/BFSGraphSearch.class

$(DPDIR)/BFSGraphSearch.class: $(DP_SRC_DIR)/BFSGraphSearch.scala
	cd $(DP_SRC_DIR); make

# Exercises

$(DIR)/TotalStack.class: $(DIR)/ImmutableStack.class

# DF search

$(DIR)/DFGraphSearch.class: $(DIR)/GraphSearch.class

$(DIR)/ConcDFGraphSearch.class: $(DIR)/DFGraphSearch.class $(DIR)/TerminatingPartialStack.class

$(DIR)/Sudoku.class: $(DIR)/ConcDFGraphSearch.class

FSC = fsc -deprecation -cp .. -d ..

$(DIR)/%.class:     %.scala
	$(FSC)  $<

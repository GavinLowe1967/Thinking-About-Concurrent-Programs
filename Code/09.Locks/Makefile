ROOT = ../tacp

DIR = $(ROOT)/locks

DTDIR = $(ROOT)/datatypes

TRAPDIR = $(ROOT)/trapezium

all: $(DIR)/Counter.class $(DTDIR)/LockTotalQueue.class $(TRAPDIR)/BagOfTasksLock.class $(DIR)/CounterArray.class  $(DTDIR)/ShardedMap.class $(DTDIR)/SetTest.class

$(DTDIR)/LinkedListSet.class: $(DTDIR)/ShardedSet.class

$(DTDIR)/SetTest.class: $(DTDIR)/ShardedSet.class $(DTDIR)/LinkedListSet.class

clean:
	rm $(DIR)/*.class; fsc -shutdown

FSC = fsc -deprecation -cp .. -d ..

$(DIR)/%.class $(DTDIR)/%.class $(TRAPDIR)/%.class:     %.scala
	$(FSC)  $<

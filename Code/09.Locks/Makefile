ROOT = ../tacp

DIR = $(ROOT)/locks

DTDIR = $(ROOT)/datatypes

TRAPDIR = $(ROOT)/trapezium

EXERCISES = $(DTDIR)/MapTest.class $(DTDIR)/LinkedListMap.class  $(DTDIR)/ShardedMap.class $(DTDIR)/ConcurrentIterator.class

all: $(DIR)/Counter.class $(DTDIR)/LockTotalQueue.class $(TRAPDIR)/BagOfTasksLock.class $(DIR)/CounterArray.class  $(DTDIR)/SetTest.class $(EXERCISES)

$(DTDIR)/LinkedListSet.class: $(DTDIR)/ShardedSet.class

$(DTDIR)/SetTest.class: $(DTDIR)/ShardedSet.class $(DTDIR)/LinkedListSet.class

# Maps

$(DTDIR)/ShardedMap.class: $(DTDIR)/ShardedSet.class

$(DTDIR)/MapTest.class: $(DTDIR)/ShardedMap.class $(DTDIR)/LinkedListMap.class

clean:
	rm $(DIR)/*.class; fsc -shutdown

FSC = fsc -deprecation -cp .. -d ..

$(DIR)/%.class $(DTDIR)/%.class $(TRAPDIR)/%.class:     %.scala
	$(FSC)  $<

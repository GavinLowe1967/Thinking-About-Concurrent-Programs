ROOT = ../tacp

DIR = $(ROOT)/jvmMonitors

DTDIR = $(ROOT)/datatypes

all: $(DIR)/Counter.class $(DTDIR)/MonitorTotalQueue.class $(DTDIR)/MonitorShardedSet.class

tQueue:  $(DTDIR)/MonitorTotalQueue.class

striped: $(DIR)/StripedCounterArray.class

shardedSet: $(DTDIR)/MonitorShardedSet.class

# Standard recipes

clean:
	rm $(DIR)/*.class; fsc -shutdown

FSC = fsc -d .. -deprecation -cp ..

$(DIR)/%.class $(DTDIR)/%.class:     %.scala
	$(FSC)  $<

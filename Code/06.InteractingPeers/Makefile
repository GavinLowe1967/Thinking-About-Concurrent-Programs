ROOT = ../tacp

DIR = $(ROOT)/interactingPeers

EXERCISES = $(DIR)/RingFold.class $(DIR)/AtomicBroadcast.class	\
  $(DIR)/GridMaxTest.class

all:   $(DIR)/SumTest.class $(DIR)/LeadershipElection.class $(DIR)/SpanningTree.class $(EXERCISES)

clean:
	rm $(DIR)/*.class $(DIR)/*/*.class; fsc -shutdown

FSC = fsc -deprecation -cp .. -d ..

$(DIR)/SumTest.class: $(DIR)/Sum.class

$(DIR)/GridMaxTest.class: $(DIR)/GridMax.class

# AtomicBroadcastTest reuses code from the clients-and-servers chapter.
$(DIR)/AtomicBroadcast.class: $(ROOT)/clientServer/AtomicBroadcastTest.class

$(ROOT)/clientServer/AtomicBroadcastTest.class:
	cd ../05.ClientServer; make

$(DIR)/%.class:     %.scala
	$(FSC)  $<

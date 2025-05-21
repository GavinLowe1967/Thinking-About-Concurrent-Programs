DIR = ../tacp/channels

all: $(DIR)/QSort.class $(DIR)/Mults4.class $(DIR)/BoundedMults4.class		\
	$(DIR)/NatsCircuit.class $(DIR)/BoundedNatsCircuit.class		\
	$(DIR)/Hamming.class $(DIR)/Sorting.class $(DIR)/Mergesort.class	\
	$(DIR)/PipeSort.class

clean:
	rm $(DIR)/*.class; fsc -shutdown

FSC = fsc -deprecation -d .. -cp .:..

$(DIR)/%.class:     %.scala
	$(FSC)  $<

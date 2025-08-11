DIR = ../tacp/datatypes

EXERCISES = $(DIR)/TotalStack.class

all: $(DIR)/QueueTest.class $(DIR)/PartialQueueTest.class $(DIR)/TerminatingPartialQueue.class $(EXERCISES)

clean:
	rm $(DIR)/*.class; fsc -shutdown

$(DIR)/QueueTest.class: $(DIR)/TotalQueue.class

$(DIR)/TotalStack.class: $(DIR)/ImmutableStack.class

$(DIR)/PartialQueueTest: $(DIR)/PartialQueue.class

FSC = fsc -deprecation -cp .. -d ..

$(DIR)/%.class:     %.scala
	$(FSC)  $<

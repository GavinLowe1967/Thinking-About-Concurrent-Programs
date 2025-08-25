DIR = ../tacp/datatypes

EXERCISES = $(DIR)/TotalStack.class

all: $(DIR)/QueueTest.class $(DIR)/PartialQueueTest.class $(DIR)/WordPath.class $(EXERCISES)

clean:
	rm $(DIR)/*.class; fsc -shutdown

$(DIR)/QueueTest.class: $(DIR)/TotalQueue.class

$(DIR)/TotalStack.class: $(DIR)/ImmutableStack.class

$(DIR)/PartialQueueTest: $(DIR)/PartialQueue.class

$(DIR)/GraphSearch.class: $(DIR)/TerminatingPartialQueue.class

$(DIR)/WordPath.class:  $(DIR)/GraphSearch.class

FSC = fsc -deprecation -cp .. -d ..

$(DIR)/%.class:     %.scala
	$(FSC)  $<

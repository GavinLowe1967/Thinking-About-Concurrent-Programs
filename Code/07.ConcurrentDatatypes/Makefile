DIR = ../tacp/datatypes

EXERCISES = $(DIR)/TotalStack.class $(DIR)/Sudoku.class $(DIR)/PartialStack.class $(DIR)/ArrayMax.class

all: $(DIR)/QueueTest.class $(DIR)/PartialQueueTest.class $(DIR)/WordPath.class $(EXERCISES)

clean:
	rm $(DIR)/*.class; fsc -shutdown

$(DIR)/QueueTest.class: $(DIR)/TotalQueue.class

$(DIR)/PartialQueueTest.class: $(DIR)/PartialQueue.class

# BF search

$(DIR)/ConcBFGraphSearch.class: $(DIR)/GraphSearch.class $(DIR)/TerminatingPartialQueue.class $(DIR)/ConcSet.class

$(DIR)/WordPath.class:  $(DIR)/ConcBFGraphSearch.class

# Exercises

$(DIR)/TotalStack.class: $(DIR)/ImmutableStack.class

# DF search

$(DIR)/DFGraphSearch.class: $(DIR)/GraphSearch.class

$(DIR)/ConcDFGraphSearch.class: $(DIR)/DFGraphSearch.class $(DIR)/TerminatingPartialStack.class

$(DIR)/Sudoku.class: $(DIR)/ConcDFGraphSearch.class

FSC = fsc -deprecation -cp .. -d ..

$(DIR)/%.class:     %.scala
	$(FSC)  $<

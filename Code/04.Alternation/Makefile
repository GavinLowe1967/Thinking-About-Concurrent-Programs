DIR = ../tacp/alternation

EXERCISES = $(DIR)/PhilsExercise.class

all:	$(DIR)/Phils.class $(DIR)/PhilsLog.class $(DIR)/Buff2.class $(DIR)/Quicksort.class $(EXERCISES)


clean:
	rm $(DIR)/*.class; fsc -shutdown

FSC = fsc -deprecation -d .. -cp .:.. 

$(DIR)/%.class:     %.scala
	$(FSC)  $<

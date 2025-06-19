DIR = ../tacp/alternation

EXERCISES = $(DIR)/PhilsExercise.class $(DIR)/Buffer.class $(DIR)/AdaptiveTest.class

all: $(DIR)/Phils.class $(DIR)/PhilsLog.class $(DIR)/Buff2.class	\
 $(DIR)/Quicksort.class $(EXERCISES)

clean:
	rm $(DIR)/*.class; fsc -shutdown

$(DIR)/AdaptiveTest.class: $(DIR)/Adaptive.class

FSC = fsc -deprecation -d .. -cp .:.. 

$(DIR)/%.class:     %.scala
	$(FSC)  $<

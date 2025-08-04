DIR = ../tacp/datatypes

all: $(DIR)/TotalQueue.class


clean:
	rm $(DIR)/*.class; fsc -shutdown

FSC = fsc -deprecation -cp .. -d ..

$(DIR)/%.class:     %.scala
	$(FSC)  $<

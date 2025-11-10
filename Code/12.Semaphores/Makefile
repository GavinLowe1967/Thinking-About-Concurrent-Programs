ROOT = ../tacp

# Directories for objects

DIR = $(ROOT)/semaphores

all: $(DIR)/Counter.class


# Standard recipes

clean:
	rm $(DIR)/*.class; fsc -shutdown

FSC = fsc -d .. -deprecation -cp ..

$(DIR)/%.class $(DTDIR)/%.class:     %.scala
	$(FSC)  $<

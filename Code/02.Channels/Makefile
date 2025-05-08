DIR = ../tacp/channels

all:	$(DIR)/QSortChannels.class $(DIR)/Mults4.class

clean:
	rm $(DIR)/*.class; fsc -shutdown

FSC = fsc -deprecation -d .. -cp .:..

$(DIR)/%.class:     %.scala
	$(FSC)  $<

DIR = ../tacp/clientServer

all:   $(DIR)/RAServer.class $(DIR)/PartialRAServer.class $(DIR)/Exchanger.class $(DIR)/FilterChan.class

# $(DIR)/RATest.class: $(DIR)/RAServer.class

clean:
	rm $(DIR)/*.class; fsc -shutdown

FSC = fsc -d .. -deprecation -cp ..

$(DIR)/%.class:     %.scala
	$(FSC)  $<

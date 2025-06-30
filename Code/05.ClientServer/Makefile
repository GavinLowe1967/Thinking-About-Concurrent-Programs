DIR = ../tacp/clientServer

EXERCISES = $(DIR)/MenWomen.class $(DIR)/AtomicBroadcast.class

all: $(DIR)/RAServer.class $(DIR)/PartialRAServer.class		\
  $(DIR)/Exchanger.class $(DIR)/FilterChan.class $(EXERCISES)

# $(DIR)/RATest.class: $(DIR)/RAServer.class

clean:
	rm $(DIR)/*.class; fsc -shutdown

FSC = fsc -d .. -deprecation -cp ..

$(DIR)/%.class:     %.scala
	$(FSC)  $<

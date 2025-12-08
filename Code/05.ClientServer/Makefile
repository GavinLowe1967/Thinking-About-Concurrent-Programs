ROOT = ../tacp

DIR = $(ROOT)/clientServer

MONDIR = $(ROOT)/monitors

MON_SRC_DIR = ../10.Conditions

EXERCISES = $(DIR)/MenWomen.class $(DIR)/AtomicBroadcastTest.class

all: $(DIR)/RAServer.class $(DIR)/PartialRAServer.class		\
  $(DIR)/ExchangerTest.class $(DIR)/FilterChan.class $(EXERCISES)

# $(DIR)/RATest.class: $(DIR)/RAServer.class

# Men and women

menWomen: $(DIR)/MenWomen.class

# Exchanger

$(DIR)/ExchangerTest.class: $(DIR)/Exchanger.class $(MONDIR)/MonitorExchanger.class

$(MONDIR)/MonitorExchanger.class: $(MON_SRC_DIR)/MonitorExchanger.scala
	cd $(MON_SRC_DIR); make exchanger

exchanger: $(DIR)/Exchanger.class

# Atomic broadcast 

$(DIR)/AtomicBroadcastTest.class: $(DIR)/AtomicBroadcast.class $(MONDIR)/AtomicBroadcast.class

$(MONDIR)/AtomicBroadcast.class: $(MON_SRC_DIR)/AtomicBroadcast.scala
	cd $(MON_SRC_DIR); make atomicBroadcast

# Standard recipes

clean:
	rm $(DIR)/*.class; fsc -shutdown

FSC = fsc -d .. -deprecation -cp ..

$(DIR)/%.class:     %.scala
	$(FSC)  $<

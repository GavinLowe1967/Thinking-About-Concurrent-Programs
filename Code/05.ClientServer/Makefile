ROOT = ../tacp

DIR = $(ROOT)/clientServer

MONDIR = $(ROOT)/monitors

MON_SRC_DIR = ../10.Conditions

EXERCISES = $(DIR)/MenWomenTest.class $(DIR)/AtomicBroadcast.class

all: $(DIR)/RAServer.class $(DIR)/PartialRAServer.class		\
  $(DIR)/ExchangerTest.class $(DIR)/FilterChan.class $(EXERCISES)

# $(DIR)/RATest.class: $(DIR)/RAServer.class

# Men and women

$(DIR)/MenWomenTest.class: $(DIR)/MenWomen.class $(MONDIR)/MenWomenM.class

$(MONDIR)/MenWomenM.class: $(MON_SRC_DIR)/MenWomenM.scala
	cd $(MON_SRC_DIR); make menWomen

menWomen: $(DIR)/MenWomen.class

# Exchanger

$(DIR)/ExchangerTest.class: $(DIR)/Exchanger.class $(MONDIR)/MonitorExchanger.class

$(MONDIR)/MonitorExchanger.class: $(MON_SRC_DIR)/MonitorExchanger.scala
	cd $(MON_SRC_DIR); make exchanger

exchanger: $(DIR)/Exchanger.class

# Standard recipe

clean:
	rm $(DIR)/*.class; fsc -shutdown

FSC = fsc -d .. -deprecation -cp ..

$(DIR)/%.class:     %.scala
	$(FSC)  $<

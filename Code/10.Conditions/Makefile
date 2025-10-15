ROOT = ../tacp

DIR = $(ROOT)/monitors

DTDIR = $(ROOT)/datatypes

CSDIR = $(ROOT)/clientServer

CS_SRC_DIR = ../05.ClientServer

EXERCISES = $(DIR)/MenWomenM.class $(DIR)/ABC.class

all: $(DTDIR)/MonitorPartialQueue.class $(DTDIR)/BoundedPartialQueueTest.class $(DIR)/SharedSyncChanTest.class $(DIR)/MonitorExchanger.class $(DIR)/BarberC.class  $(EXERCISES)

$(DTDIR)/BoundedPartialQueueTest.class:  $(DTDIR)/BoundedMonitorPartialQueue.class

$(DTDIR)/MonitorPartialQueue.class $(DTDIR)/BoundedMonitorPartialQueue.class: $(DTDIR)/PartialQueue.class

$(DIR)/SharedSyncChanTest.class: $(DIR)/SharedSyncChan.class

# Men and women problem

$(DIR)/MenWomenM.class: $(CSDIR)/MenWomen.class

$(CSDIR)/MenWomen.class: $(CS_SRC_DIR)/MenWomen.scala
	cd $(CS_SRC_DIR); make menWomen

menWomen: $(DIR)/MenWomenM.class # called from ClientServer directory

$(DIR)/MonitorExchanger.class: $(CSDIR)/Exchanger.class

$(CSDIR)/Exchanger.class: $(CS_SRC_DIR)/Exchanger.scala
	cd $(CS_SRC_DIR); make exchanger

exchanger: $(DIR)/MonitorExchanger.class

# Standard recipes

clean:
	rm $(DIR)/*.class; fsc -shutdown

queue: $(DTDIR)/MonitorPartialQueue.class

FSC = fsc -deprecation -cp .. -d ..

$(DIR)/%.class $(DTDIR)/%.class $(TRAPDIR)/%.class:     %.scala
	$(FSC)  $<


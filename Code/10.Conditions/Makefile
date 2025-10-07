ROOT = ../tacp

DIR = $(ROOT)/monitors

DTDIR = $(ROOT)/datatypes

CSDIR = $(ROOT)/clientServer

CS_SRC_DIR = ../05.ClientServer

EXERCISES = $(DIR)/MenWomenM.class

all: $(DTDIR)/MonitorPartialQueue.class $(DTDIR)/BoundedPartialQueueTest.class $(DIR)/SharedSyncChanTest.class $(DIR)/BarberC.class $(EXERCISES)

$(DTDIR)/BoundedPartialQueueTest.class:  $(DTDIR)/BoundedMonitorPartialQueue.class

$(DTDIR)/MonitorPartialQueue.class $(DTDIR)/BoundedMonitorPartialQueue.class: $(DTDIR)/PartialQueue.class

$(DIR)/SharedSyncChanTest.class: $(DIR)/SharedSyncChan.class

$(DIR)/MenWomenM.class: $(CSDIR)/MenWomen.class

$(CSDIR)/MenWomen.class: $(CS_SRC_DIR)/MenWomen.scala
	cd $(CS_SRC_DIR); make menWomen

menWomen: $(DIR)/MenWomenM.class

clean:
	rm $(DIR)/*.class; fsc -shutdown

queue: $(DTDIR)/MonitorPartialQueue.class

FSC = fsc -deprecation -cp .. -d ..

$(DIR)/%.class $(DTDIR)/%.class $(TRAPDIR)/%.class:     %.scala
	$(FSC)  $<


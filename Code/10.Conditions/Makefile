ROOT = ../tacp

# Directories for targets

DIR = $(ROOT)/monitors
DTDIR = $(ROOT)/datatypes
CSDIR = $(ROOT)/clientServer
JVM_MON_DIR = $(ROOT)/jvmMonitors

# Directories for source code

CS_SRC_DIR = ../05.ClientServer
DT_SRC_DIR = ../07.ConcurrentDatatypes
JVM_MON_SRC_DIR = ../11.JVMMonitors

EXERCISES = $(DTDIR)/MonitorBoundedPartialStack.class $(DIR)/MenWomenM.class \
  $(DIR)/ABC.class $(DIR)/AtomicBroadcast.class

QUEUES = $(DTDIR)/MonitorPartialQueue.class	\
  $(DTDIR)/BoundedPartialQueueTest.class

all: $(QUEUES) $(DIR)/SharedSyncChanTest.class $(DIR)/MonitorExchanger.class	\
  $(DIR)/ConditionsBarrier.class $(DIR)/BarberC.class				\
  $(DIR)/ReadersWritersTest.class $(EXERCISES)

# Partial queue

$(DTDIR)/MonitorPartialQueue.class $(DTDIR)/BoundedMonitorPartialQueue.class: $(DTDIR)/PartialQueue.class

$(DTDIR)/BoundedPartialQueueTest.class:  $(DTDIR)/BoundedMonitorPartialQueue.class $(DTDIR)/JVMMonitorBoundedPartialQueue.class

$(DTDIR)/JVMMonitorBoundedPartialQueue.class: $(JVM_MON_SRC_DIR)/JVMMonitorBoundedPartialQueue.scala
	cd $(JVM_MON_SRC_DIR); make bpQueue

# Synchronous channel.

$(DIR)/SharedSyncChanTest.class: $(DIR)/SharedSyncChan.class $(JVM_MON_DIR)/JVMMonitorSyncChan.class

$(JVM_MON_DIR)/JVMMonitorSyncChan.class: $(JVM_MON_SRC_DIR)/JVMMonitorSyncChan.scala
	cd $(JVM_MON_SRC_DIR); make syncChan

syncChan: $(DIR)/SharedSyncChan.class

# Men and women problem

$(DIR)/MenWomenM.class: $(CSDIR)/MenWomen.class

$(CSDIR)/MenWomen.class: $(CS_SRC_DIR)/MenWomen.scala
	cd $(CS_SRC_DIR); make menWomen

menWomen: $(DIR)/MenWomenM.class # called from ClientServer directory

# Exchanger

$(DIR)/MonitorExchanger.class: $(CSDIR)/Exchanger.class

$(CSDIR)/Exchanger.class: $(CS_SRC_DIR)/Exchanger.scala
	cd $(CS_SRC_DIR); make exchanger

exchanger: $(DIR)/MonitorExchanger.class

# Partial stack

$(DTDIR)/MonitorBoundedPartialStack.class: $(DTDIR)/PartialStack.class $(DTDIR)/ImmutableStack.class 

$(DTDIR)/PartialStack.class $(DTDIR)/ImmutableStack.class: 
	cd $(DT_SRC_DIR); make

# Atomic broadcast
$(DIR)/AtomicBroadcast.class: $(CSDIR)/AtomicBroadcast.class

$(CSDIR)/AtomicBroadcast.class: $(CS_SRC_DIR)/AtomicBroadcast.scala
	cd $(CS_SRC_DIR); make

atomicBroadcast: $(DIR)/AtomicBroadcast.class

# Readers and writers
$(DIR)/ReadersWritersTest.class: $(DIR)/ReadersWriters.class

# Standard recipes

clean:
	rm $(DIR)/*.class; fsc -shutdown

queue: $(DTDIR)/MonitorPartialQueue.class

FSC = fsc -deprecation -cp .. -d ..

$(DIR)/%.class $(DTDIR)/%.class $(TRAPDIR)/%.class:     %.scala
	$(FSC)  $<


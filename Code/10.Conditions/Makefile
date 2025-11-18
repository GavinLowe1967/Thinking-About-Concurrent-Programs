ROOT = ../tacp

# Directories for targets

DIR = $(ROOT)/monitors
DPDIR = $(ROOT)/dataParallel
DTDIR = $(ROOT)/datatypes
CSDIR = $(ROOT)/clientServer
JVM_MON_DIR = $(ROOT)/jvmMonitors

# Directories for source code

CS_SRC_DIR = ../05.ClientServer
DT_SRC_DIR = ../07.ConcurrentDatatypes
DP_SRC_DIR = ../08.DataParallel
JVM_MON_SRC_DIR = ../11.JVMMonitors

EXERCISES = $(DTDIR)/MonitorBoundedPartialStack.class $(DIR)/MenWomenM.class \
  $(DIR)/ABC.class $(DIR)/AtomicBroadcast.class

QUEUES = $(DTDIR)/MonitorPartialQueue.class	\
  $(DTDIR)/BoundedPartialQueueTest.class

all: $(QUEUES) $(DIR)/MonitorSyncChan.class $(DIR)/MonitorExchanger.class	\
  $(DIR)/ConditionsBarrier.class $(DIR)/BarberC.class				\
  $(DIR)/ReadersWritersLock.class $(EXERCISES)

# Partial queue

$(DTDIR)/MonitorPartialQueue.class $(DTDIR)/BoundedMonitorPartialQueue.class: $(DTDIR)/PartialQueue.class

$(DTDIR)/BoundedPartialQueueTest.class:  $(DTDIR)/BoundedMonitorPartialQueue.class $(DTDIR)/JVMMonitorBoundedPartialQueue.class

$(DTDIR)/JVMMonitorBoundedPartialQueue.class: $(JVM_MON_SRC_DIR)/JVMMonitorBoundedPartialQueue.scala
	cd $(JVM_MON_SRC_DIR); make bpQueue

# Barrier synchronisation

$(DIR)/ConditionsBarrier.class: $(DPDIR)/ServerBarrier.class

$(DPDIR)/ServerBarrier.class: $(DP_SRC_DIR)/ServerBarrier.scala
	cd $(DP_SRC_DIR); make barrier

barrier: $(DIR)/ConditionsBarrier.class

# Synchronous channel.

$(DIR)/MonitorSyncChan.class: $(DIR)/SyncChanT.class

syncChan: $(DIR)/MonitorSyncChan.class

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

$(DIR)/MonitorReadersWriters.class: $(DIR)/ReadersWritersLock.class

readersWriters: $(DIR)/MonitorReadersWriters.class

# Standard recipes

clean:
	rm $(DIR)/*.class; fsc -shutdown

queue: $(DTDIR)/MonitorPartialQueue.class

FSC = fsc -deprecation -cp .. -d ..

$(DIR)/%.class $(DTDIR)/%.class $(TRAPDIR)/%.class:     %.scala
	$(FSC)  $<


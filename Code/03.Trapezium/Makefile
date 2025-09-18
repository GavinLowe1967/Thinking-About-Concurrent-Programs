ROOT = ../tacp

DIR = $(ROOT)/trapezium

LOCK_SRC_DIR = ../09.Locks

EXERCISES =  $(DIR)/Pi.class $(DIR)/Matrix.class

all:   $(DIR)/TrapeziumTest.class $(DIR)/TrapeziumExperiment.class $(EXERCISES)

clean:
	rm $(DIR)/*.class; fsc -shutdown

FSC = fsc -deprecation -d .. -cp .:.. 

# FSCX = fsc  -deprecation -d .. -cp .:..
# :/home/gavin/Scala/Util

$(DIR)/Trapezium.class: $(DIR)/TrapeziumT.class

$(DIR)/TrapeziumBag.class: $(DIR)/Trapezium.class

$(DIR)/TrapeziumBagObjects.class: $(DIR)/TrapeziumT.class $(DIR)/BagOfTasksLock.class

$(DIR)/BagOfTasksLock.class: $(LOCK_SRC_DIR)/BagOfTasksLock.scala
	cd $(LOCK_SRC_DIR); make

$(DIR)/TrapeziumTest.class $(DIR)/TrapeziumRun.class: $(DIR)/Trapezium.class $(DIR)/TrapeziumBag.class $(DIR)/TrapeziumBagObjects.class

$(DIR)/TrapeziumExperiment.class: $(DIR)/TrapeziumRun.class

# $(DIR)/TrapeziumExperiment.class: TrapeziumExperiment.scala
# 	$(FSCX) $<
# $(DIR)/TrapeziumRun.class: TrapeziumRun.scala
# 	$(FSCX) $<
# TrapeziumExperiment.scala

$(DIR)/%.class:     %.scala
	$(FSC)  $<

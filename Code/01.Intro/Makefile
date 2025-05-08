DIR = ../tacp/race

all:   $(DIR)/Race.class $(DIR)/BusyWait.class

clean:
	rm $(DIR)/*.class; fsc -shutdown

FSC = fsc -d .. -cp .:..
#/home/gavin/Scala/SCL

$(DIR)/%.class:     %.scala
	$(FSC)  $<

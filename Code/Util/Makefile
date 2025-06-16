ROOT = ../tacp/util

CP = .:..
# /home/gavin/Scala/Util

EXPDIR = $(ROOT)/experiments
EXPS = $(EXPDIR)/SpecialFunctions.class $(EXPDIR)/Experiments.class $(EXPDIR)/Graphs.class


PROFILEDIR = $(ROOT)/profiling
PROFILERS = $(PROFILEDIR)/Profiler.class $(PROFILEDIR)/SamplingProfiler.class	\
$(PROFILEDIR)/InstrumentationAgent.class					\
$(PROFILEDIR)/InstrumentationAgent.jar $(PROFILEDIR)/MemoryProfiler.class

all: 	$(EXPS)  $(PROFILERS)

doc:
	scaladoc -d ../doc experiments/*.scala profiling/*.scala

clean:
	rm $(ROOT)/*/*.class; fsc -shutdown

$(EXPDIR)/specialFunctions.class: experiments/specialFunctions.java
	javac -d .. experiments/specialFunctions.java; fsc -shutdown
# It is necessary to do fsc -shutdown after compiling specialFunctions and
# before compiling ConfidenceIntervals.scala

$(EXPDIR)/SpecialFunctions.class: $(EXPDIR)/specialFunctions.class

$(EXPDIR)/ConfidenceIntervals.class: $(EXPDIR)/SpecialFunctions.class

$(EXPDIR)/Experiments.class: $(EXPDIR)/ConfidenceIntervals.class

$(PROFILEDIR)/Profiler.class: $(PROFILEDIR)/ThreadID.class

$(PROFILEDIR)/InstrumentationAgent.class: profiling/InstrumentationAgent.java
	javac -d .. profiling/InstrumentationAgent.java

$(PROFILEDIR)/InstrumentationAgent.jar: $(PROFILEDIR)/InstrumentationAgent.class
	cp profiling/MANIFEST.MF $(PROFILEDIR);
	cd $(PROFILEDIR); \
	jar -cmf MANIFEST.MF InstrumentationAgent.jar InstrumentationAgent.class; \
	fsc -shutdown
# It's necessary to do fsc -shutdown after creating InstrumentationAgent.jar
# and before compiling MemoryProfiler.

$(PROFILEDIR)/MemoryProfiler.class: $(PROFILEDIR)/InstrumentationAgent.jar

$(ROOT)/%.class: %.scala
	 fsc -cp $(CP) -deprecation -d .. $<

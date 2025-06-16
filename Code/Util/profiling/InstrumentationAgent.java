package tacp.util.profiling;

import java.lang.instrument.Instrumentation;

/** Object to support memory profiling. */
public class InstrumentationAgent {
  private static volatile Instrumentation globalInstrumentation;
 
  public static void premain(final String agentArgs, final Instrumentation inst){
    globalInstrumentation = inst;
  }
 
  /** Get the direct size of object. */
  public static long getObjectSize(final Object object) {
    if (globalInstrumentation == null) {
      throw new IllegalStateException("Agent not initialized.");
    }
    return globalInstrumentation.getObjectSize(object);
  }
}


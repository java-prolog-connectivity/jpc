package org.jpc.util;


import javafx.application.Application;
import javafx.application.Platform;
import javafx.stage.Stage;

/**
 * A utility class for facilitating launching JavaFX applications from the Prolog side.
 * This class is not intended to be used directly from the Java side.
 * @author sergioc
 *
 */
public class JavaFXLauncher extends Application {

	private static boolean launched;
	private static final Object appInitLock = new Object();
	
	private static class WrappedStage {
		Stage stage;
		Exception ex;
	}
	
    public static Stage show(Class<? extends Stage> stageClass) {
    	launchIfNeeded();
    	final Object stageInitLock = new Object();
    	WrappedStage wStage = new WrappedStage();

    	javafx.application.Platform.runLater(new Runnable() {
    		Stage stage;
			@Override
			public void run() {
				synchronized(stageInitLock) {
					try {
						stage = stageClass.newInstance(); //this must be executed in the user interface thread.
					} catch (InstantiationException | IllegalAccessException e) {
						wStage.ex = e;
						throw new RuntimeException(e);
					} finally {
						stageInitLock.notify();
					}
				}
				wStage.stage = stage;
				stage.show();
			}
		});
    	synchronized(stageInitLock) {
    		while(wStage.stage == null && wStage.ex != null) {
        		try {
    				stageInitLock.wait();
    			} catch (InterruptedException e) {
    				throw new RuntimeException(e);
    			}
        	}
    	}
    	return wStage.stage;
    }
    
	@Override
	public void init() {
		synchronized(appInitLock) {
			Platform.setImplicitExit(false);
			launched = true;
			appInitLock.notifyAll();
		}
	}
	
	public static void launchIfNeeded() {
		synchronized(appInitLock) {
			if(!launched) {
				new Thread() {
					@Override
					public void run() {
						try {
							Application.launch(JavaFXLauncher.class);
						} catch(Exception e) {
							System.out.println(e);
							throw(e);
						}
						
					}
				}.start();
				
				while(!launched) {
					try {
						appInitLock.wait();
					} catch (InterruptedException e) {
						throw new RuntimeException(e);
					}
				}
			}
		}
	}
	
    @Override
    public void start(Stage primaryStage) {
    }

}

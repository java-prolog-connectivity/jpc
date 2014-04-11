package org.jpc.util;


import javafx.application.Application;
import javafx.application.Platform;
import javafx.stage.Stage;

/**
 * A utility class for facilitating launching JavaFX applications from the Prolog side.
 * @author sergioc
 *
 */
public class JavaFXLauncher extends Application {

	private static boolean launched;
	private static Object lock = new Object();
	
    public static void show(Class<? extends Stage> stageClass) {
    	launchIfNeeded();
    	javafx.application.Platform.runLater(new Runnable() {	
			@Override
			public void run() {
				Stage stage;
				try {
					stage = stageClass.newInstance();
				} catch (InstantiationException | IllegalAccessException e) {
					throw new RuntimeException(e);
				}
				stage.show();
			}
		});
    }
    
	@Override
	public void init() {
		synchronized(lock) {
			Platform.setImplicitExit(false);
			launched = true;
			lock.notifyAll();
		}
	}
	
	public static void launchIfNeeded() {
		synchronized(lock) {
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
						lock.wait();
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

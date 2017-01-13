package org.jpc.internal.io;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;

public class FileUtil {

	/*
	public static void copy(String resource, Path to) {
		try(InputStream in = getClass().getClassLoader().getResourceAsStream(resource);) {
			Files.copy(in, to);
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}
	*/
	
	/**
	 * DISCLAIMER: this method and its comments have been adapted from an answer to this question: http://stackoverflow.com/questions/779519/delete-files-recursively-in-java
	 * @param path the folder to delete
	 * @throws IOException if a file or folder cannot be deleted
	 */
	public static void deleteRecursively(Path path) throws IOException {
	    Files.walkFileTree(path, new SimpleFileVisitor<Path>()
	    {
	        @Override
	        public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
	            Files.delete(file);
	            return FileVisitResult.CONTINUE;
	        }

	        @Override
	        public FileVisitResult visitFileFailed(Path file, IOException exc) throws IOException {
	            Files.delete(file); // try to delete the file anyway, even if its attributes could not be read, since delete-only access is theoretically possible
	            return FileVisitResult.CONTINUE;
	        }

	        @Override
	        public FileVisitResult postVisitDirectory(Path dir, IOException exc) throws IOException {
	            if (exc == null) {
	                Files.delete(dir);
	                return FileVisitResult.CONTINUE;
	            }
	            else { //something went wrong
	                throw exc; //propagate the exception
	            }
	        }
	    });
	}
	
	/**
	 * 
	 * @param url the url to analyze
	 * @return a boolean indicating if a url points to a file directly located in the file system (not inside a jar for example)
	 */
	public static boolean isFileSystemDir(URL url) {
		try {
			Path path = new File(url.toURI()).toPath();
			return Files.isDirectory(path);
		} catch (Exception e) {
			return false;
		}
		/*
		String protocol = url.getProtocol();
		if(protocol != null) {
			return protocol.matches("^file");
		}
		return false;
		*/
	}
}

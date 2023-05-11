package com.vz.backend.core.thread;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Comparator;
import java.util.Date;
import java.util.stream.Stream;

import org.springframework.stereotype.Component;

import com.vz.backend.core.service.FTPService;
import com.vz.backend.util.DateTimeUtils;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Component
public class HandleTrashCan implements Runnable {
	@Override
	public void run() {
		String dir = FTPService.getTrashPath(DateTimeUtils.getDayBefore(new Date()));
		log.info("Start delete trash can in folder : " + dir);
		this.delDir(dir);
	}

	private void delDir(String dir) {
		Path path = Paths.get(dir);
		// read java doc, Files.walk need close the resources.
		// try-with-resources to ensure that the stream's open directories are closed
		try (Stream<Path> walk = Files.walk(path)) {
			walk.sorted(Comparator.reverseOrder()).forEach(HandleTrashCan::delDirExtract);
			log.info("Delete trash can in folder : " + dir + " sucessfully");
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	// extract method to handle exception in lambda
	private static void delDirExtract(Path path) {
		try {
			Files.deleteIfExists(path);
		} catch (IOException e) {
			System.err.printf("Unable to delete this path : %s%n%s", path, e);
		}
	}

}

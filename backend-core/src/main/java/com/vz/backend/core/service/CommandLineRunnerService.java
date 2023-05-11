package com.vz.backend.core.service;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.autoconfigure.SpringBootApplication;

@SpringBootApplication
public class CommandLineRunnerService implements CommandLineRunner{

	@Autowired
	private UserService userService;
	
	@Autowired
	private ModuleService moduleService;

	@Override
	public void run(String... args) throws Exception {
		userService.createAdmin();
		moduleService.inactiveCabinetAdminModule();
	}
}

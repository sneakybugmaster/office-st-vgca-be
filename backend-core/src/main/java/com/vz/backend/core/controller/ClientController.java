package com.vz.backend.core.controller;

import java.util.List;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.vz.backend.core.domain.Client;
import com.vz.backend.core.repository.IClientRepository;

@RestController
@RequestMapping("/clients")
public class ClientController {
	@Autowired
	private IClientRepository clientRepository;

	@PostMapping()
	public ResponseEntity<Client> createClient(@RequestBody Client client) {
		try {
			return new ResponseEntity<>(clientRepository.save(client), HttpStatus.OK);
		} catch (Exception e) {
			return ResponseEntity.badRequest().build();
		}
	}

	@GetMapping("")
	public ResponseEntity<List<Client>> getAllClients() {
		try {
			return new ResponseEntity<>(clientRepository.findAll(), HttpStatus.OK);
		} catch (Exception e) {
			return ResponseEntity.badRequest().build();
		}
	}

	@GetMapping("/{id}")
	public ResponseEntity<Client> getClientId(@PathVariable long id) {
		try {
			Optional<Client> client = clientRepository.findById(id);
			if (client.isPresent()) {
				return new ResponseEntity<>(client.get(), HttpStatus.OK);
			}
			return ResponseEntity.status(HttpStatus.NOT_FOUND).build();

		} catch (Exception e) {
			return ResponseEntity.badRequest().build();
		}
	}

	@PutMapping("/{id}")
	public ResponseEntity<Client> updateClientId(@PathVariable long id, @RequestBody Client requestClient) {
		try {
			Optional<Client> client = clientRepository.findById(id);
			if (client.isPresent()) {
				requestClient.setId(client.get().getId());
				return new ResponseEntity<>(clientRepository.save(requestClient), HttpStatus.OK);
			}
			return ResponseEntity.status(HttpStatus.NOT_FOUND).build();

		} catch (Exception e) {
			return ResponseEntity.badRequest().build();
		}
	}
}

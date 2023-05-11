package com.vz.backend.core.controller;

import java.util.List;

import javax.servlet.http.HttpServletRequest;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;

import com.vz.backend.core.auth.SecurityContext;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.domain.RootModel;
import com.vz.backend.core.domain.User;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.service.IService;

import lombok.extern.slf4j.Slf4j;

/**
 * @author DucND
 * @date Apr 14, 2020
 */
@Slf4j
public abstract class BaseController<T extends RootModel> {

	@Autowired
	HttpServletRequest request;

	protected User getCurrentUser() {
		return SecurityContext.getCurrentUser();
	}

	public BaseController() {
	}

	public Long getClientId() {
		User user = getCurrentUser();
		if (user != null) {
			return user.getClientId();
		}
		throw new RestExceptionHandler(Message.NOT_FOUND_CLIENT);
	}

	public abstract IService<T> getService();

	/**
	 * API return all object
	 *
	 * @return
	 */
	@GetMapping("/getxxx")
	public ResponseEntity<List<T>> getFull() {
		try {
			return new ResponseEntity<>(getService().findAll(), HttpStatus.OK);
		} catch (Exception e) {
			return ResponseEntity.badRequest().build();
		}
	}

	@GetMapping("/getAll")
	public ResponseEntity<?> getByClientId() {
		List<T> data = getService().findByClientId(getClientId());
		return new ResponseEntity<>(data, HttpStatus.OK);
	}

	@GetMapping("/getAll/{page}")
	public ResponseEntity<?> getByClientId(@PathVariable int page) {
		List<T> data = getService().findByClientId(getClientId(), page);
		log.info("Get all response:" + data.toString());
		return new ResponseEntity<>(data, HttpStatus.OK);
	}

	@GetMapping("/getAllSort/{direction}/{column}")
	public ResponseEntity<?> getByClientIdAndSort(@PathVariable String direction,
			@RequestParam(required = false) Boolean active, @PathVariable String column) {
		Sort sort;
		if (direction.equals("ASC")) {
			sort = Sort.by(Direction.ASC, column);
		} else {
			sort = Sort.by(Direction.DESC, column);
		}
		List<T> data = getService().findByClientIdAndActive(getClientId(), active, sort);
		return new ResponseEntity<>(data, HttpStatus.OK);
	}

	@GetMapping("/getActive")
	public ResponseEntity<?> getActive() {
		List<T> data = getService().findByClientIdAndActive(getClientId(), true);
		return new ResponseEntity<>(data, HttpStatus.OK);
	}

	@GetMapping("/getActive/{page}")
	public ResponseEntity<?> getActive(@PathVariable int page) {
		List<T> data = getService().findByClientIdAndActive(getClientId(), true, page);
		return new ResponseEntity<>(data, HttpStatus.OK);
	}

	@GetMapping("/getById/{id}")
	public ResponseEntity<?> getById(@PathVariable Long id) {

		T data = getService().findByClientIdAndId(getClientId(), id);
		return new ResponseEntity<>(data, HttpStatus.OK);
	}

	@PostMapping("/add")
	public ResponseEntity<?> create(@RequestBody T input) {
		input = getService().save(input);
		return new ResponseEntity<>(input, HttpStatus.OK);
	}

	@PostMapping("/update/{id}")
	public ResponseEntity<?> update(@PathVariable Long id, @RequestBody T input) {
		T data = getService().findByClientIdAndId(getClientId(), id);
		if (data != null) {
			input.setId(data.getId());
		}
		data = getService().save(input);
		return new ResponseEntity<>(data, HttpStatus.OK);
	}

	@GetMapping("/deactive/{id}")
	public ResponseEntity<?> deactive(@PathVariable Long id) {
		T data = getService().findByClientIdAndId(getClientId(), id);
		if (data == null || !data.getActive()) {
			log.error("Data object is null");
			return new ResponseEntity<>(HttpStatus.NOT_FOUND);
		} else {
			data.setActive(false);
			data = getService().save(data);
			return new ResponseEntity<>(data, HttpStatus.OK);
		}
	}

	@GetMapping("/active/{id}")
	public ResponseEntity<?> active(@PathVariable Long id) {
		T data = getService().findByClientIdAndId(getClientId(), id);
		if (data == null || data.getActive() != null && data.getActive()) {
			log.error("Data object is null");
			return new ResponseEntity<>(HttpStatus.NOT_FOUND);
		} else {
			data.setActive(true);
			data = getService().save(data);
			return new ResponseEntity<>(data, HttpStatus.OK);
		}
	}

	@PostMapping("/delete/{id}")
	public ResponseEntity<?> delete(@PathVariable Long id) {
		try {
			this.getService().deleteById(id);
			return ResponseEntity.status(HttpStatus.OK).build();
		} catch (Exception e) {
			return ResponseEntity.badRequest().build();
		}
	}
}

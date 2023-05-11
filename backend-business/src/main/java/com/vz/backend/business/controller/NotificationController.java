package com.vz.backend.business.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.vz.backend.business.domain.Notification;
import com.vz.backend.business.service.NotificationService;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.service.IService;

@RestController
@RequestMapping("/notification")
public class NotificationController {

	@Autowired
	NotificationService notiService;

	// @Override
	public IService<Notification> getService() {
		return notiService;
	}

	@RequestMapping("/get")
	public ResponseEntity<?> get() {
		return new ResponseEntity<>(notiService.get(BussinessCommon.getUser().getId()), HttpStatus.OK);
	}
	@RequestMapping("/getTotal")
	public ResponseEntity<?> getTotal() {
		return new ResponseEntity<>(notiService.getTotal(BussinessCommon.getUser().getId()), HttpStatus.OK);
	}

	@RequestMapping("/countUnread")
	public ResponseEntity<?> countUnread() {
		return new ResponseEntity<>(notiService.countUnread(BussinessCommon.getUser().getId()), HttpStatus.OK);
	}

	@PostMapping("/setRead/{id}")
	public ResponseEntity<?> setRead(@PathVariable Long id) {
		return new ResponseEntity<>(notiService.setRead(id), HttpStatus.OK);
	}

	@PostMapping("/delete/{id}")
	public ResponseEntity<?> delete(@PathVariable Long id) {
		return new ResponseEntity<>(notiService.delete(id), HttpStatus.OK);
	}

	@PostMapping("/deleteAll")
	public ResponseEntity<?> deleteAll() {
		return new ResponseEntity<>(notiService.deactiveAllByUserId(BussinessCommon.getUser().getId()), HttpStatus.OK);
	}

	/*
	 * @PostMapping("/deleteAllByIds") public ResponseEntity<?>
	 * deleteAllByIds(@RequestParam Long[] listIds) { return new
	 * ResponseEntity<>(notiService.deactiveAllByIds(listIds), HttpStatus.OK); }
	 */

	@RequestMapping("/checkModule/{id}")
	public ResponseEntity<?> checkModule(@PathVariable Long id) {
		return new ResponseEntity<>(notiService.checkModule(id), HttpStatus.OK);
	}
}

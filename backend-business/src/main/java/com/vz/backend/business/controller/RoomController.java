package com.vz.backend.business.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import com.vz.backend.business.domain.Room;
import com.vz.backend.business.service.RoomService;
import com.vz.backend.core.controller.BaseController;
import com.vz.backend.core.service.IService;

@RestController
@RequestMapping("/room")
public class RoomController extends BaseController<Room> {

	@Autowired
	RoomService roomService;

	@Override
	public IService<Room> getService() {
		return roomService;
	}

	@GetMapping(value = "/getRoomByOrg/{orgId}")
	public ResponseEntity<?> getRoomByOrg(@PathVariable long orgId) {
		return new ResponseEntity<>(roomService.findByOrgIdAndActive(orgId), HttpStatus.OK);
	}

	@PostMapping(value = "/addRoom")
	public ResponseEntity<?> addRoom(@RequestBody Room room) {
		room.setActive(true);
		roomService.addRoom(room);
		return new ResponseEntity<>(HttpStatus.OK);
	}

	@GetMapping
	public ResponseEntity<Iterable<Room>> getAllRoom() {
		return new ResponseEntity<>(roomService.findAll(), HttpStatus.OK);
	}

	@PostMapping(value = "/updateRoom/{id}")
	public ResponseEntity<?> updateRoom(@RequestBody Room room, @PathVariable Long id) {
		roomService.updateRoom(room, id);
		return new ResponseEntity<>(HttpStatus.OK);
	}

}

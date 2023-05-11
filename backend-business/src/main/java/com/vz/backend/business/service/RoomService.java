package com.vz.backend.business.service;

import java.util.List;

import javax.persistence.EntityManagerFactory;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.vz.backend.business.domain.Room;
import com.vz.backend.business.repository.IRoomRepository;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.service.BaseService;

@Service
public class RoomService extends BaseService<Room> {

	@Autowired
	IRoomRepository roomRepository;

	@Autowired
	EntityManagerFactory entityManagerFactory;

	@Override
	public IRepository<Room> getRepository() {
		return roomRepository;
	}

	public List<Room> findByOrgIdAndActive(long orgId) {
		return roomRepository.findByOrgIdAndActive(orgId, true);
	}

	public void addRoom(Room room) {
		roomRepository.save(room);
	}

	public Room updateRoom(Room room, Long id) {
		room.setId(id);
		roomRepository.save(room);
		return room;
	}
}

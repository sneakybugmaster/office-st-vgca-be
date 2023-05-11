package com.vz.backend.business.service;

import java.util.*;

import com.vz.backend.business.domain.Room;
import com.vz.backend.business.dto.calendar.RoomStatisticDto;
import com.vz.backend.core.config.CalendarStatusEnum;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import com.vz.backend.business.domain.RoomMeeting;
import com.vz.backend.business.dto.MeetingTimeDto;
import com.vz.backend.business.repository.ICalendar2Repository;
import com.vz.backend.business.repository.IRoomMeetingRepository;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.service.BaseService;

@Service
public class RoomMeetingService extends BaseService<RoomMeeting>{

	@Autowired
	IRoomMeetingRepository roomRepository;
	
	@Autowired
	ICalendar2Repository calendar2Repository;
	
	@Override
	public IRepository<RoomMeeting> getRepository() {
		return this.roomRepository;
	}
	
	public RoomMeeting add(RoomMeeting room) {
		room.valid();
		return roomRepository.save(room);
	}

	public List<RoomMeeting> getRoom() {
		List<RoomMeeting> meetingRooms = roomRepository.findByClientIdAndActiveTrue(BussinessCommon.getClientId());
		List<MeetingTimeDto> meetingTimes = findMeetingTime();
		return set(meetingRooms, meetingTimes);
	}
	
	public Page<RoomMeeting> getRoomPage(String text, String name, String address, String description, Integer quantity,
			Float acreage, Pageable page) {
		if(text != null) { //for basic
			return roomRepository.getRoomPageByCondition(text, BussinessCommon.getClientId(), page);
		}
		
		return roomRepository.getRoomPageByCondition(name, address, description, quantity, acreage, BussinessCommon.getClientId(), page);
	}
	
	private List<RoomMeeting> set(List<RoomMeeting> meetingRooms, List<MeetingTimeDto> meetingTimes) {
		if (meetingRooms.isEmpty() || meetingTimes.isEmpty())
			return meetingRooms;

		Map<Long, List<MeetingTimeDto>> map = new HashMap<>();
		meetingRooms.forEach(i -> map.put(i.getId(), new ArrayList<>()));

		meetingTimes.forEach(i -> {
			Long key = i.getRoomId();
			if (!map.containsKey(key)) {
				map.put(key, new ArrayList<>());
			}
			map.get(key).add(i);
		});

		meetingRooms.forEach(j -> {
			Long key = j.getId();
			j.setMeetingTimes(map.get(key));
		});

		return meetingRooms;
	}

	public List<MeetingTimeDto> findMeetingTime() {
		return calendar2Repository.findMeetingTime(BussinessCommon.getClientId());
	}

	public RoomMeeting inActive(Long id) {
		RoomMeeting room = roomRepository.findByClientIdAndId(BussinessCommon.getClientId(), id);
		if(room == null) throw new RestExceptionHandler(Message.ROOM_NOT_FOUND);
		
		boolean calendarUserRoom = calendar2Repository.countByRoom(id, BussinessCommon.getClientId());
		if(calendarUserRoom) throw new RestExceptionHandler(Message.ROOM_USING);
		
		room.setActive(false);
		return roomRepository.save(room);
	}

	public Page<RoomStatisticDto> getStatistic(Date startDate, Date endDate, Long roomId, Long chairmanId, String name, CalendarStatusEnum status, Pageable pageable) {
		return roomRepository.getStatistic(startDate, endDate, BussinessCommon.getClientId(), roomId, chairmanId, name.toLowerCase(), status, pageable);
	}

	public List<RoomMeeting> getAllByName(String name) {
		return roomRepository.getAllByName(BussinessCommon.getClientId(), true, name.toLowerCase());
	}
}

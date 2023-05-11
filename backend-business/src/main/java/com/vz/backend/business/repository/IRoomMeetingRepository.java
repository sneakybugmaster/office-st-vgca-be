package com.vz.backend.business.repository;

import java.util.List;

import com.vz.backend.business.domain.Room;
import com.vz.backend.business.dto.calendar.RoomStatisticDto;
import com.vz.backend.core.config.CalendarStatusEnum;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.vz.backend.business.domain.RoomMeeting;
import com.vz.backend.core.repository.IRepository;
import java.util.Date;

@Repository
public interface IRoomMeetingRepository extends IRepository<RoomMeeting>{

	List<RoomMeeting> findByClientIdAndActiveTrue(Long clientId);

	@Query("select r from RoomMeeting r "
			+ "where lower(r.name) like  %:text% or lower(r.description) like %:text% or lower(r.address) like %:text% "
			+ "and r.clientId=:clientId and r.active is true")
	Page<RoomMeeting> getRoomPageByCondition(String text, Long clientId, Pageable page);

	@Query("select r from RoomMeeting r "
			+ "where (:name is null or lower(r.name) like  %:name%) "
			+ "and (:description is null or lower(r.description) like %:description%) "
			+ "and (:address is null or lower(r.address) like %:address%) "
			+ "and (:quantity is null or r.quantity = :quantity) "
			+ "and (:acreage is null or r.acreage = :acreage) "
			+ "and r.clientId=:clientId and r.active is true ")
	Page<RoomMeeting> getRoomPageByCondition(String name, String address, String description, Integer quantity,
			Float acreage, Long clientId, Pageable page);

	@Query("SELECT new com.vz.backend.business.dto.calendar.RoomStatisticDto(r.name, r.address, count(c.id)) FROM RoomMeeting r inner join Calendar2 c on r.id = c.roomId"
            + " WHERE r.active = true and c.active = true and r.clientId = :clientId and c.clientId = : clientId"
            + " AND (COALESCE(:startDate, NULL) IS NULL OR  c.createDate >= :startDate)"
			+ " AND (COALESCE(:endDate, NULL) IS NULL OR c.createDate <= :endDate)"
            + " AND (:chairmanId is null or c.chairmanId = :chairmanId) "
            + " AND (:roomId is null or c.roomId = :roomId) and c.isMeetingCalendar = true "
			+ " AND lower(r.name) like  %:name% or lower(r.description) like %:name% or lower(r.address) like %:name%"
			+ " AND c.status = :status"
			+ " GROUP BY r.name, r.address")
    Page<RoomStatisticDto> getStatistic(Date startDate, Date endDate, Long clientId, Long roomId, Long chairmanId, String name, CalendarStatusEnum status, Pageable page);

	@Query("SELECT r FROM RoomMeeting r WHERE r.clientId = :clientId and r.active = :active and lower(r.name) like %:name% ")
	List<RoomMeeting> getAllByName(Long clientId, Boolean active, String name);

}

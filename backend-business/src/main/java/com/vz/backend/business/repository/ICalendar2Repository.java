package com.vz.backend.business.repository;

import com.vz.backend.business.domain.Calendar2;
import com.vz.backend.business.domain.ecabinet.Meeting;
import com.vz.backend.business.dto.MeetingTimeDto;
import com.vz.backend.core.config.CalendarStatusEnum;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.domain.User;
import com.vz.backend.core.repository.IRepository;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.Date;
import java.util.List;

@Repository
public interface ICalendar2Repository extends IRepository<Calendar2> {

	@Query("SELECT c FROM Calendar2 c WHERE" 
			+ " (c.startTime BETWEEN :startDate AND :endDate OR c.endTime BETWEEN :startDate AND :endDate)"
			+ " AND (:clientId is NULL or c.clientId = :clientId)" 
			+ " AND c.active IS TRUE"
			+ " AND c.orgId in (:orgIds)"
			+ " AND c.status in (:status)"
			+ " AND (c.registerBan = :registerBan OR c.isUnitCalendar = :isUnitCalendar )"
			+ " AND c.isMeetingCalendar IS FALSE")
	Page<Calendar2> findByTopLevel(boolean registerBan, boolean isUnitCalendar, Date startDate, Date endDate, List<Long> orgIds, List<CalendarStatusEnum> status,
			Long clientId, Pageable pageable);
	
	@Query("SELECT c FROM Calendar2 c WHERE" 
			+ " (c.startTime BETWEEN :startDate AND :endDate OR c.endTime BETWEEN :startDate AND :endDate)"
			+ " AND (:clientId is NULL or c.clientId = :clientId)" 
			+ " AND c.active IS TRUE"
			+ " AND c.orgId in (:orgIds)"
			+ " AND c.status in (:status)"
			+ " AND (c.registerBan = :registerBan OR c.isUnitCalendar = :isUnitCalendar )"
			+ " AND c.isMeetingCalendar IS FALSE")
	List<Calendar2> findByTopLevel(boolean registerBan, boolean isUnitCalendar, Date startDate, Date endDate, List<Long> orgIds, List<CalendarStatusEnum> status,
			Long clientId);
	
	@Query("SELECT c FROM Calendar2 c WHERE" 
			+ " (c.startTime BETWEEN :startDate AND :endDate OR c.endTime BETWEEN :startDate AND :endDate)"
			+ " AND (:clientId is NULL or c.clientId = :clientId)" 
			+ " AND c.active IS TRUE"
			+ " AND c.orgId in (:orgIds)"
			+ " AND c.registerBan = :registerBan"
			+ " AND c.isUnitCalendar IS TRUE "
			+ " AND c.status in (:status)"
			+ " AND c.isMeetingCalendar IS FALSE")
	Page<Calendar2> findByUnitLevel(boolean registerBan, Date startDate, Date endDate, List<Long> orgIds, List<CalendarStatusEnum> status,
			Long clientId, Pageable pageable);
	
	@Query("SELECT c FROM Calendar2 c WHERE" 
			+ " (c.startTime BETWEEN :startDate AND :endDate OR c.endTime BETWEEN :startDate AND :endDate)"
			+ " AND (:clientId is NULL or c.clientId = :clientId)" 
			+ " AND c.active IS TRUE"
			+ " AND (c.orgId in (:orgIds))"
			+ " AND c.registerBan = :registerBan"
			+ " AND c.isUnitCalendar IS TRUE"
			+ " AND c.status in (:status)"
			+ " AND c.isMeetingCalendar IS FALSE"
			+ " OR c.id IN (SELECT c1.id FROM Calendar2 c1 WHERE c1.clientId=:clientId AND c1.active IS TRUE AND c.isUnitCalendar IS FALSE AND c1.createBy=:userId AND c1.status in (:status) AND c1.isMeetingCalendar IS FALSE AND :registerBan IS TRUE )")
	List<Calendar2> findByUnitLevel(Long userId, boolean registerBan, Date startDate, Date endDate, List<Long> orgIds, List<CalendarStatusEnum> status,
			Long clientId);
	
	@Query("SELECT c FROM Calendar2 c"
			+ " WHERE (c.startTime BETWEEN :startDate AND :endDate OR c.endTime BETWEEN :startDate AND :endDate)"
			+ " AND c.orgId in (:orgId)" + " AND c.clientId = :clientId" + " AND c.active = :active"
			//+ " AND c.status='APPROVE'" 
			+ " AND c.registerBan is FALSE"
			+ " AND c.orgModel.orgTypeModel.name != 'Ban'"
			+ " AND ( c.createBy=:userId or :checkPermission is true or c.status='APPROVE' ) "
			+ " AND c.isMeetingCalendar IS FALSE and c.isUnitCalendar is true")
	List<Calendar2> findByMonthOfCuc(Date startDate, Date endDate, List<Long> orgId, Long clientId, boolean active,Long userId,Boolean checkPermission,  Sort sort);

	@Query("SELECT c FROM Calendar2 c"
			+ " WHERE (c.startTime BETWEEN :startDate AND :endDate OR c.endTime BETWEEN :startDate AND :endDate)"
			+ " AND c.clientId = :clientId" + " AND c.active = :active" //+ " AND c.status = 'APPROVE'"
			+ " AND ((c.orgModel.orgTypeModel.name = 'Ban' ) "
			+ "       OR (c.orgModel.orgTypeModel.name != 'Ban' AND c.registerBan is TRUE ) "
			+ "  	  OR (lower(c.orgModel.name) = 'phòng tổng hợp - văn phòng ban') "
			+ ") AND ( c.createBy=:userId or :checkPermission is true or c.status='APPROVE')"
			+ " AND c.isMeetingCalendar IS FALSE and c.isUnitCalendar is false")
	List<Calendar2> findByMonthOfBan(Date startDate, Date endDate,  Long clientId,
			boolean active,Long userId,Boolean checkPermission, Sort sort);

	Calendar2 findByIdAndClientIdAndActive(Long id, Long clientId, boolean active);

	@Query("SELECT cj.calendar FROM CalendarJoin2 cj where cj.active is TRUE and cj.calendar.active is TRUE "
			+ "and :year*:y + :month*:m BETWEEN YEAR(cj.calendar.startTime)*:y + MONTH(cj.calendar.startTime)*:m "
			+ "AND YEAR(cj.calendar.endTime)*:y + MONTH(cj.calendar.endTime)*:m "
			+ "and cj.userId=:userId AND cj.clientId=:clientId")
	List<Calendar2> findIsInvitation(Integer month, Integer year, int m, int y, Long userId, Long clientId);

	@Query("SELECT count(c) > 0 FROM Calendar2 c WHERE" 
			+ " (c.startTime BETWEEN :startDate AND :endDate OR c.endTime BETWEEN :startDate AND :endDate)"
			+ " AND (:clientId is NULL or c.clientId = :clientId)" 
			+ " AND c.active IS TRUE"
			+ " AND c.roomId =:roomId"
			+ " AND c.isMeetingCalendar IS TRUE")
	boolean isRoomTimeDuplicate(Long roomId, Date startDate, Date endDate, Long clientId);

	@Query("SELECT new com.vz.backend.business.dto.MeetingTimeDto(c.roomId, c.startTime, c.endTime) FROM Calendar2 c " 
			+ " WHERE c.endTime > NOW()"
			+ " AND (:clientId is NULL or c.clientId = :clientId)" 
			+ " AND c.active IS TRUE"
			+ " AND c.isMeetingCalendar IS TRUE"
			+ " GROUP BY c.roomId, c.startTime, c.endTime")
	List<MeetingTimeDto> findMeetingTime(Long clientId);

	@Query("SELECT distinct(c) FROM Calendar2 c LEFT JOIN CalendarJoin2 j ON j.calendarId=c.id AND j.clientId =c.clientId "
			+ " join Calendar2Ingredient ci on c.id = ci.calendarId WHERE (c.startTime BETWEEN :startDate AND :endDate OR c.endTime BETWEEN :startDate AND :endDate)"
			+ " AND (c.clientId = :clientId )"
			+ " AND (c.active IS TRUE OR j.active IS TRUE)"
			+ " AND ((:userId IS NULL OR c.status = 'APPROVE' and (c.chairmanId = :userId) or (j.userId = :userId or ci.objectId = :userId or"
			+ " (ci.type = 'ORG' and :userId in"
			+ " (select u.id from User u where u.org = ci.objectId and u.active = true and u.lead is true))))"
			+ " OR (:creator IS NULL OR c.createBy = :creator))"
//			+ " AND ((:status) IS NULL OR c.status IN (:status))"
			+ " AND (:roomId IS NULL OR c.roomId = :roomId)"
			+ " AND (:userIdJoin IS NULL OR j.userId = :userIdJoin)"
			+ " AND c.isMeetingCalendar IS TRUE ORDER BY c.startTime ASC")
	List<Calendar2> findMeetingCalendar(Long userId, Long creator, Date startDate, Date endDate, Long clientId, Long roomId, Long userIdJoin);

	@Query("SELECT count(1) > 0 FROM Calendar2 c" 
			+ " WHERE c.clientId = :clientId" 
			+ " AND c.active IS TRUE"
			+ " AND c.roomId = :id"
			+ " AND c.isMeetingCalendar IS TRUE")
	boolean countByRoom(Long id, Long clientId);

	@Query("SELECT distinct(c) FROM Calendar2 c LEFT JOIN CalendarJoin2 j ON j.calendarId=c.id" 
			+ " WHERE (c.startTime >= :startDate)"
			+ " AND (c.clientId = :clientId AND j.clientId = :clientId)" 
			+ " AND c.active IS TRUE AND j.active IS TRUE"
			+ " AND (:userId IS NULL OR j.userId = :userId OR c.createBy =:userId)"
			+ " AND c.isMeetingCalendar IS TRUE"
			)
	List<Calendar2> getToNow(Date startDate, Long userId, Long clientId);

	List<Calendar2> findByClientIdAndActiveTrueAndIdIn(Long clientId, List<Long> ids);

	@Query("SELECT COUNT(1) > 0 FROM Calendar2 c "
			+ " LEFT JOIN CalendarJoin2 cj ON cj.calendarId = c.id "
			+ " LEFT JOIN DocumentCalendar dc ON dc.cId = c.id "
			+ " WHERE c.clientId =:clientId AND c.active=TRUE AND cj.clientId =:clientId AND cj.active=TRUE AND dc.clientId =:clientId AND dc.active=TRUE "
			+ " AND dc.type =:type AND (cj.userId in (:userIds) OR c.createBy in (:userIds)) AND (dc.docInId = :docId OR dc.docOutId = :docId)")
	boolean isMemberRelatedDoc(List<Long> userIds, Long docId, Long clientId, DocumentTypeEnum type);

	@Query("SELECT COUNT(1) > 0 FROM Calendar2 c "
			+ " WHERE c.clientId =:clientId AND c.active=TRUE AND c.id = :calId"
			+ " AND (c.createBy = :#{#user.id} OR c.participants LIKE %:#{#user.fullName}%)")
	boolean isMemberCalendar(User user, Long calId, Long clientId);

	@Query("SELECT c FROM Calendar2 c "
			+ " WHERE ((DATE(c.startTime) =:date) or (DATE(c.endTime) = :date) or ( (DATE(c.startTime) <=:date) and (DATE(c.endTime) >= :date)))"
			+ " AND c.clientId = :clientId AND (:statusEnum is null or c.status=:statusEnum) AND (:roomId is null or c.roomId=:roomId) "
			+ " AND c.active IS TRUE ANd c.isMeetingCalendar is true ORDER BY c.roomMeeting.name, c.startTime"
	)
	List<Calendar2> getCalendar2toDate(Date date, Long clientId, CalendarStatusEnum statusEnum, Long roomId);

	@Query("SELECT mt FROM Meeting mt WHERE mt.active = TRUE AND mt.clientId = :clientId AND mt.calendarId = :calendarId")
	Meeting getMeetingByCalendarId(Long clientId, Long calendarId);

	@Query("SELECT distinct(c) FROM Calendar2 c LEFT JOIN CalendarJoin2 j ON j.calendarId=c.id AND j.clientId =c.clientId "
			+ " join Calendar2Ingredient ci on c.id = ci.calendarId WHERE (c.startTime BETWEEN :startDate AND :endDate OR c.endTime BETWEEN :startDate AND :endDate)"
			+ " AND (c.clientId = :clientId )"
			+ " AND (c.active IS TRUE OR j.active IS TRUE)"
			+ " AND ((:userId IS NULL OR (c.chairmanId = :userId or j.userId = :userId or ci.objectId = :userId or"
			+ " (ci.type = 'ORG' and :userId in"
			+ " (select u.id from User u where u.org = ci.objectId and u.active = true and u.lead is true)))))"
			+ " AND c.status = 'APPROVE'"
			+ " AND c.isMeetingCalendar IS TRUE ORDER BY c.startTime ASC")
	List<Calendar2> findMeetingPerson(Long userId, Date startDate, Date endDate, Long clientId);
}

package com.vz.backend.business.repository.ecabinet;

import java.util.Date;
import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.vz.backend.business.domain.Calendar2;
import com.vz.backend.business.domain.ecabinet.Agenda;
import com.vz.backend.business.domain.ecabinet.Meeting;
import com.vz.backend.business.dto.ecabinet.CalendarResult;
import com.vz.backend.business.dto.ecabinet.CalendarSearch;
import com.vz.backend.business.dto.ecabinet.MeetingDto;
import com.vz.backend.business.dto.ecabinet.MeetingSearchDto;
import com.vz.backend.core.domain.User;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface IMeetingRepository extends IRepository<Meeting> {

	@Query("SELECT COUNT(1) > 0 FROM Meeting m WHERE m.placeId = :placeId AND (:start BETWEEN m.start AND m.end) AND m.clientId=:clientId AND m.active=TRUE AND m.status NOT IN ('DONG', 'DA_HOP')")
	Boolean findByPlaceIdAndTime(Long placeId, Date start, Long clientId);

	public static final String FIND_ALL_QUERY = "SELECT NEW com.vz.backend.business.dto.ecabinet.MeetingDto(m, at)"
			+ " FROM Meeting m "
			+ " LEFT JOIN Attendance at ON at.meetingId = m.id AND at.active = TRUE AND (at.userId = :#{#dto.userId} OR at.replaceUserId = :#{#dto.userId} OR (:#{#dto.bossId} IS NOT NULL AND at.userId = :#{#dto.bossId} AND at.invitation ='UNIT') OR (:#{#dto.bossId} IS NOT NULL AND at.replaceUserId = :#{#dto.bossId} AND at.invitation ='UNIT' ))"
			+ " LEFT JOIN Agenda ag ON ag.meetingId = m.id AND ag.active = TRUE "
			+ " LEFT JOIN Participant p ON p.meetingId = m.id AND p.active = TRUE "
			+ " WHERE m.clientId = :clientId AND m.active = TRUE "
			+ " AND (:#{#dto.meetingId} IS NULL OR m.id = :#{#dto.meetingId})"
			+ " AND (:#{#dto.text} IS NULL "
			+ " OR LOWER(m.subject) LIKE %:#{#dto.text}% "
//			+ " OR LOWER(m.roomMeeting.name) LIKE %:#{#dto.text}% "
			+ " OR LOWER(m.schedule) LIKE %:#{#dto.text}%) "
			+ " AND (COALESCE(:#{#dto.start}, NULL) IS NULL OR m.start > :#{#dto.start}) "
			+ " AND (COALESCE(:#{#dto.end}, NULL) IS NULL OR m.end < :#{#dto.end}) "
			+ " AND (:#{#dto.content} IS NULL "
			+ " OR LOWER(m.subject) LIKE %:#{#dto.content}% "
			+ " OR LOWER(m.roomMeeting.name) LIKE %:#{#dto.content}% "
			+ " OR LOWER(m.schedule) LIKE %:#{#dto.content}%) "
			+ " AND (:#{#dto.hostOrgId} IS NULL OR ag.hostId = :#{#dto.hostOrgId}) "
			+ " AND (:#{#dto.hostUserId} IS NULL OR (p.objId = :#{#dto.hostUserId} AND p.type != 'ORG' AND p.role = 'HOST')) "
			+ " AND (:#{#dto.memberId} IS NULL OR m.id IN (SELECT at2.meetingId FROM Attendance at2 WHERE at2.userId = :#{#dto.memberId} AND at2.clientId = :clientId AND at2.active = TRUE)) "
			+ " AND ((m.createBy = :#{#dto.userId} OR ag.expertId = :#{#dto.userId}) OR (m.id) IN "
			+ " (SELECT at1.meetingId FROM Attendance at1 "
			+ " LEFT JOIN Secretary s1 ON (s1.bossId = at1.userId OR s1.bossId = at1.replaceUserId) AND s1.active = TRUE "
			+ " WHERE at1.meeting.createBy = :#{#dto.userId} OR at1.userId = :#{#dto.userId} OR at1.replaceUserId = :#{#dto.userId} OR s1.userId = :#{#dto.userId}))"
			+ " GROUP BY m.id, at.id"
			+ " ORDER BY m.createDate DESC ";
	
	@Query(FIND_ALL_QUERY)
	Page<MeetingDto> list(MeetingSearchDto dto, Long clientId, Pageable castToPageable);
	
	@Query(FIND_ALL_QUERY)
	List<MeetingDto> list(MeetingSearchDto dto, Long clientId);

	@Query(value = "SELECT m FROM Meeting m WHERE m.clientId = :clientId AND m.active = true AND m.subject LIKE %:subject%")
	List<Meeting> findByClientIdAndSubjectAndActiveTrue(Long clientId, String subject);

	@Query(value = "SELECT m FROM Meeting m LEFT JOIN Attendance a ON m.id = a.meetingId WHERE m.clientId = :clientId AND m.active = true AND a.userId = :userId AND m.subject LIKE %:subject%")
	List<Meeting> findByClientIdAndSubjectAndUserIdActiveTrue(Long clientId, String subject, Long userId);

	@Query("SELECT NEW com.vz.backend.business.dto.ecabinet.CalendarResult(m.id, m.subject, m.roomMeeting.name, m.start, m.end) "
			+ " FROM Meeting m "
			+ " LEFT JOIN Agenda ag ON ag.meetingId = m.id AND ag.active = TRUE "
			+ " WHERE m.clientId = :clientId AND m.active = TRUE "
			+ " AND (:#{#dto.text} IS NULL "
			+ " OR LOWER(m.subject) LIKE %:#{#dto.text}% "
//			+ " OR LOWER(m.roomMeeting.name) LIKE %:#{#dto.text}% "
			+ " OR LOWER(m.schedule) LIKE %:#{#dto.text}%) "
			+ " AND (COALESCE(:#{#dto.start}, NULL) IS NULL OR m.start > :#{#dto.start}) "
			+ " AND (COALESCE(:#{#dto.end}, NULL) IS NULL OR m.end < :#{#dto.end}) "
			+ " AND ((m.createBy = :#{#dto.userId} OR ag.expertId = :#{#dto.userId}) OR (m.id) IN (SELECT at1.meetingId FROM Attendance at1 WHERE at1.meeting.createBy = :#{#dto.userId} OR at1.userId = :#{#dto.userId} OR at1.replaceUserId = :#{#dto.userId}))"
			+ " GROUP BY m.id, m.subject, m.roomMeeting.name, m.start, m.end"
			+ " ORDER BY m.createDate DESC "
			)
	List<CalendarResult> calendar(CalendarSearch dto, Long clientId);

	List<Meeting> findByClientIdAndYearBookIdAndActiveTrue(Long clientId, Long yearBookId);
	
	public static final String FIND_BY_DRAFT = "SELECT NEW com.vz.backend.business.dto.ecabinet.MeetingDto(m.id, m.start, m.end, m.subject, m.roomMeeting.name, m.status, m.createBy)"
			+ " FROM Meeting m "
			+ " LEFT JOIN Agenda ag ON ag.meetingId = m.id AND ag.active = TRUE "
			+ " LEFT JOIN Participant p ON p.meetingId = m.id AND p.active = TRUE "
			+ " LEFT JOIN Attendance at ON at.meetingId = m.id AND at.active = TRUE "
			+ " WHERE m.clientId = :clientId AND m.active = TRUE "
			+ " AND m.status != 'MOI_TAO'"
			+ " AND (:#{#dto.text} IS NULL "
			+ " OR LOWER(m.subject) LIKE %:#{#dto.text}% "
			+ " OR LOWER(m.schedule) LIKE %:#{#dto.text}%) "
			+ " AND (COALESCE(:#{#dto.start}, NULL) IS NULL OR m.start > :#{#dto.start}) "
			+ " AND (COALESCE(:#{#dto.end}, NULL) IS NULL OR m.end < :#{#dto.end}) "
			+ " AND (:#{#dto.content} IS NULL "
			+ " OR LOWER(m.subject) LIKE %:#{#dto.content}% "
			+ " OR LOWER(m.roomMeeting.name) LIKE %:#{#dto.content}% "
			+ " OR LOWER(m.schedule) LIKE %:#{#dto.content}%) "
			+ " AND (:#{#dto.hostOrgId} IS NULL OR ag.hostId = :#{#dto.hostOrgId}) "
			+ " AND (:#{#dto.hostUserId} IS NULL OR (p.objId = :#{#dto.hostUserId} AND p.type != 'ORG' AND p.role = 'HOST')) "
			+ " AND (:#{#dto.memberId} IS NULL OR at.userId = :#{#dto.memberId}) "
			+ " AND ("
//			+ "	(m.createBy = :#{#dto.userId} OR ag.expertId = :#{#dto.userId}) OR "
			+ "	(m.id) IN "
			+ " (SELECT p.meetingId FROM Participant p "
			+ " LEFT JOIN Secretary s1 ON (s1.bossId = p.objId) AND s1.active = TRUE AND p.active = TRUE AND p.role = 'HOST' AND p.type != 'ORG' AND p.clientId = s1.clientId "
			+ " WHERE s1.userId = :#{#dto.userId} AND s1.clientId = :clientId)) "
//			+ " AND (m.id) NOT IN (SELECT d.meetingId FROM Draft d WHERE d.clientId =:clientId AND d.active = TRUE )"
			+ " GROUP BY m.id, m.start, m.end, m.subject, m.roomMeeting.name, m.status"
			+ " ORDER BY m.createDate DESC ";
	
	@Query(FIND_BY_DRAFT)
	Page<MeetingDto> list4Draft(MeetingSearchDto dto, Long clientId, Pageable castToPageable);
	
	@Query(FIND_BY_DRAFT)
	List<MeetingDto> list4Draft(MeetingSearchDto dto, Long clientId);

	Meeting findByClientIdAndIdAndActiveTrue(Long clientId, Long objId);

	@Query("SELECT m FROM Meeting m "
			+ " LEFT JOIN Agenda ag ON ag.meetingId = m.id AND ag.active = TRUE "
			+ " WHERE m.clientId = :clientId AND m.active = TRUE AND m.yearBookId IN (:ybIds)"
			+ " AND (m.createBy = :userId OR ag.expertId = :userId OR (m.id) IN "
			+ " (SELECT at1.meetingId FROM Attendance at1 "
			+ " LEFT JOIN Secretary s1 ON (s1.bossId = at1.userId OR s1.bossId = at1.replaceUserId) AND s1.active = TRUE AND at1.clientId = :clientId "
			+ " WHERE at1.userId = :userId OR at1.replaceUserId = :userId OR s1.userId = :userId)"
			+ " )"
			)
	List<Meeting> getMeetingIdByYearBookIds(List<Long> ybIds, Long userId, Long clientId);

	@Query("SELECT NEW com.vz.backend.business.domain.Calendar2(m) "
			+ " FROM Meeting m "
			+ " LEFT JOIN Agenda ag ON ag.meetingId = m.id AND ag.active = TRUE "
			+ " WHERE m.clientId = :clientId AND m.active = TRUE "
			+ " AND (COALESCE(:start, NULL) IS NULL OR m.start > :start) "
			+ " AND (COALESCE(:end, NULL) IS NULL OR m.end < :end) "
			+ " AND ((m.createBy = :userId OR ag.expertId = :userId) OR (m.id) IN (SELECT at1.meetingId FROM Attendance at1 WHERE at1.meeting.createBy = :userId OR at1.userId = :userId OR at1.replaceUserId = :userId))"
			+ " GROUP BY m.id, m.subject, m.roomMeeting.name, m.start, m.end"
			+ " ORDER BY m.createDate DESC "
			)
	List<Calendar2> findMeetingCalendar(Long clientId, Long userId, Date start, Date end);
}

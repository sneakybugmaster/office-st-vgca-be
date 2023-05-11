package com.vz.backend.business.repository;

import java.util.List;

import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import com.vz.backend.business.domain.ScheduleRemind;
import com.vz.backend.business.dto.MatchScheduleDto;
import com.vz.backend.business.dto.ScheduleRemindAssignedDto;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface IScheduleRemindRepository extends IRepository<ScheduleRemind> {

	@Query("SELECT new com.vz.backend.business.dto.ScheduleRemindAssignedDto(sr.id, sr.description, sr.hours, sr.dayOfWeek, sr.date, sr.month, sr.position, sr.userId, sr.active, sr.objType, sr.objId) "
			+ "FROM ScheduleRemind sr WHERE sr.createBy=:userId and sr.clientId=:clientId "
			+ "AND (:active is NULL or sr.active=:active)")
	List<ScheduleRemindAssignedDto> getAll(Long userId, Boolean active, Long clientId);

	@Query("SELECT new com.vz.backend.business.dto.ScheduleRemindAssignedDto(sr.id, sr.description, sr.hours, sr.dayOfWeek, sr.date, sr.month, sr.position, sr.userId, sri.active, false, sr.objType, sr.objId) "
			+ "FROM ScheduleRemind sr JOIN User user ON user.id=:id JOIN Organization org ON org.id=user.org "
			+ "LEFT JOIN Organization orgParent ON user.lead is TRUE and org.parentId=orgParent.id "
			+ "LEFT JOIN User userParent ON userParent.org=orgParent.id "
			+ "LEFT JOIN User userLead ON userLead.org=org.id "
			+ "LEFT JOIN ScheduleRemindIgnore sri ON sri.remindId=sr.id AND sri.active is TRUE AND sri.userId=user.id "
			+ "WHERE (sr.createBy=userLead.id OR sr.createBy=userParent.id) "
			+ "AND (sr.position='*' OR sr.position LIKE CONCAT('%,', user.position,',%')) "
			+ "AND (sr.userId='*' OR sr.userId LIKE CONCAT('%,', user.id,',%')) "
			+ "AND sr.active is TRUE "
			+ "GROUP BY sr.id, sr.description, sr.hours, sr.dayOfWeek, sr.date, sr.month, sr.position, sr.userId, sri.active")
	List<ScheduleRemindAssignedDto> assigned(Long id);

	@Query("SELECT sr.createBy FROM ScheduleRemind sr WHERE sr.id=:id")
	Long getOwner(Long id);

	@Query("SELECT sr FROM ScheduleRemind sr WHERE sr.id=:id")
	ScheduleRemind getOwnerFull(Long id);

	
	@Modifying
	@Query("DELETE FROM ScheduleRemindIgnore sri WHERE sri.remindId=:id")
	void deleteIgnore(Long id);

	@Query("SELECT new com.vz.backend.business.dto.MatchScheduleDto(sr.clientId, userSame.id, sr.id, sr.description) "
			+ "FROM ScheduleRemind sr JOIN User createUser ON createUser.id=sr.createBy "
			+ "JOIN Organization org ON org.id=createUser.org OR org.parentId =createUser.org "
			+ "JOIN User userSame ON userSame.org=org.id "
			+ "LEFT JOIN ScheduleRemindIgnore sri ON sri.remindId=sr.id AND sri.active is TRUE AND sri.userId=userSame.id "
			+ "WHERE (sr.position='*' OR sr.position LIKE CONCAT('%,', userSame.position,',%')) "
			+ "AND (sr.userId='*' OR sr.userId LIKE CONCAT('%,', userSame.id,',%')) "
			+ "AND sr.active is TRUE AND sri.active is NULL "
			+ "AND (userSame.org=createUser.org OR userSame.lead is TRUE)")
	List<MatchScheduleDto> match(String hour, String date, String dayOFWeek, String month);
}

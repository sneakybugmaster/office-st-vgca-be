package com.vz.backend.business.repository;

import java.util.List;

import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.vz.backend.business.domain.AttachmentCalendar;
import com.vz.backend.core.config.ObjTypeEnum;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface IAttachmentCalendarRepository extends IRepository<AttachmentCalendar>{

	AttachmentCalendar findByNameAndClientIdAndActiveTrue(String name, Long clientId);

	@Query("SELECT a FROM AttachmentCalendar a "
			+ "WHERE a.clientId = :clientId AND a.objType = :type AND (:objId IS NULL OR a.objId = :objId) AND a.active = TRUE")
	List<AttachmentCalendar> findByObjIdAndObjType(Long objId, ObjTypeEnum type, Long clientId);

	List<AttachmentCalendar> findByObjTypeAndClientIdAndActiveTrueAndObjIdIn(ObjTypeEnum type, Long clientId, List<Long> objId);

	AttachmentCalendar findByClientIdAndWeekAndYearAndObjTypeAndActiveTrue(Long clientId, int week, int year, ObjTypeEnum type);
}

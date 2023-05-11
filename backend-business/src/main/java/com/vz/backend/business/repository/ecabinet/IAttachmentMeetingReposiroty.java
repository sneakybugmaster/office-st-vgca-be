package com.vz.backend.business.repository.ecabinet;

import java.util.List;

import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.vz.backend.business.config.ecabinet.FileTypeEnum;
import com.vz.backend.business.domain.AttachmentCalendar;
import com.vz.backend.business.domain.ecabinet.AttachmentMeeting;
//import com.vz.backend.business.dto.ecabinet.AttachmentMeetingDto;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface IAttachmentMeetingReposiroty extends IRepository<AttachmentMeeting> {

	AttachmentMeeting findByNameAndFileTypeAndClientIdAndActiveTrue(String name, FileTypeEnum type, Long clientId);

	List<AttachmentMeeting> findByNameInAndFileTypeAndClientIdAndActiveTrue(List<String> names, FileTypeEnum type,
			Long clientId);

	List<AttachmentMeeting> findByNameInAndFileTypeAndFileStatusAndActiveTrue(List<String> names, FileTypeEnum type,
			FileTypeEnum status);

	@Query("SELECT a From AttachmentMeeting a WHERE a.clientId = :clientId AND a.objId = :objId")
	List<AttachmentMeeting> findByClientIdAndObjIdAndFileStatus(Long clientId, Long objId);

	List<AttachmentMeeting> findByObjIdAndFileStatusAndActiveTrue(Long objId, FileTypeEnum status);
	
	@Query(value = "SELECT atm FROM AttachmentMeeting atm WHERE atm.objId = :meetingId AND atm.clientId = :clientId AND atm.active = 'TRUE' AND atm.fileType != 'AGENDA'")
	List<AttachmentMeeting> findByObjIdAndClientId(Long meetingId, Long clientId);

	@Query(value = "SELECT atm FROM AttachmentMeeting atm LEFT JOIN Agenda a ON atm.objId = a.id WHERE a.meetingId = :meetingId AND atm.fileType = 'AGENDA'")
	List<AttachmentMeeting> findAgendaAttachmentByMeetingId(Long meetingId);

	@Query(value = "SELECT atm FROM AttachmentMeeting atm LEFT JOIN Agenda a ON atm.objId = a.id AND a.active = TRUE "
			+ "LEFT JOIN Meeting m ON a.meetingId = m.id AND m.active = TRUE "
			+ "WHERE a.clientId=:clientId AND a.active =:active AND atm.fileType = 'AGENDA' AND (:subject IS NULL OR LOWER(m.subject) LIKE %:subject%)")
	List<AttachmentMeeting> findAgendaAttachmentByMeetingSubject(Long clientId, boolean active, String subject);

	List<AttachmentMeeting> findByClientIdAndActiveAndObjIdNotNull(Long clientId, boolean b);

	@Query(value = "SELECT a FROM AttachmentMeeting a LEFT JOIN Meeting m ON a.objId = m.id WHERE a.clientId=:clientId AND a.active =:active AND (a.fileType = 'INVITATION' OR a.fileType = 'SCHEDULE') AND (:subject IS NULL OR LOWER(m.subject) LIKE %:subject%)")
	List<AttachmentMeeting> findByClientIdAndActiveAndSubjectAndObjIdNotNull(Long clientId, boolean active,
			String subject);

	List<AttachmentMeeting> findByClientIdAndObjIdAndFileType(Long clientId, Long objId, FileTypeEnum fileType);
	AttachmentMeeting getByClientIdAndObjIdAndFileTypeAndActiveTrue(Long clientId, Long objId, FileTypeEnum fileType);

	AttachmentMeeting findByClientIdAndIdAndActiveTrueAndObjIdNotNull(Long clientId, Long id);

//	@Query("SELECT NEW com.vz.backend.business.dto.ecabinet.AttachmentMeetingDto(a.id, a.name) FROM AttachmentMeeting a WHERE a.clientId = :clientId AND a.objId = :objId AND a.fileType = 'MEETING_DRAFT'")
//	List<AttachmentMeetingDto> findDraftAttachmentsByMeetingId(Long clientId, Long objId);

	AttachmentMeeting findByClientIdAndObjIdAndFileTypeAndFileStatusAndActiveTrue(Long clientId, Long objId,
			FileTypeEnum agenda, FileTypeEnum additional);

	List<AttachmentMeeting> findByObjIdAndFileTypeAndClientIdAndActiveTrue(Long objId, FileTypeEnum fileType,
			Long clientId);

	@Query("SELECT a FROM AttachmentMeeting a WHERE a.clientId = :clientId AND a.objId = :objId AND a.fileType = 'COMMENT_DRAFT' ORDER BY a.createDate ASC")
	List<AttachmentMeeting> findAttachmeetingByCommentId(Long clientId, Long objId);

	AttachmentMeeting findByClientIdAndIdAndActiveTrue(Long clientId, Long id);

	@Query("SELECT a FROM AttachmentMeeting a WHERE a.clientId = :clientId AND a.objId = :objId AND a.fileStatus = :fileStatus AND a.fileApprovement != 'DRAFT' AND a.active = 'TRUE' ORDER BY a.createDate DESC, a.fileApprovement ASC")
	List<AttachmentMeeting> findByClientIdAndObjIdAndFileStatusAndActiveTrueOrderByCreateDateASC(Long clientId, Long objId,
			FileTypeEnum fileStatus);
	
	@Query("SELECT a FROM AttachmentMeeting a WHERE a.clientId = :clientId AND a.objId = :objId AND a.fileStatus = :fileStatus AND (a.createBy = :createBy OR a.fileApprovement = 'APPROVE') AND a.active = 'TRUE' ORDER BY a.createDate DESC, a.fileApprovement ASC")
	List<AttachmentMeeting> findByClientIdAndObjIdAndFileStatusAndCreateByAndActiveTrueOrderByCreateDateASC(
			Long clientId, Long objId, Long createBy, FileTypeEnum fileStatus);

	@Query("SELECT a FROM AttachmentMeeting a WHERE a.clientId = :clientId AND a.active = TRUE AND (:text IS NULL OR LOWER(a.name) LIKE %:text%) AND (a.objId IN (:meetingIds) OR a.objId IN (:agendaIds))")
	List<AttachmentMeeting> searchByText(String text, List<Long> meetingIds, List<Long> agendaIds, Long clientId);

	@Query("SELECT NEW com.vz.backend.business.domain.AttachmentCalendar(a.id, a.name) FROM AttachmentMeeting a WHERE a.clientId = :clientId AND a.fileType = :fileType AND a.objId IN :agendaIds AND a.active IS TRUE")
	List<AttachmentCalendar> findbyCLientIdAndFileTypeAndObjIdInAndActiveTrue(Long clientId, FileTypeEnum fileType,
			List<Long> agendaIds);

	List<AttachmentMeeting> findByClientIdAndObjIdInAndFileTypeInAndActiveTrue(Long clientId, List<Long> ids,
			FileTypeEnum[] fileTypes);

	AttachmentMeeting findByClientIdAndObjIdAndFileTypeAndActiveTrue(Long clientId, Long objId,
			FileTypeEnum fileType);
}

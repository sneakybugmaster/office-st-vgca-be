package com.vz.backend.business.repository.ecabinet;

import java.util.List;

import javax.transaction.Transactional;

import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.vz.backend.business.config.ReceiveTypeEnum;
import com.vz.backend.business.config.ecabinet.RoleEnum;
import com.vz.backend.business.domain.ecabinet.Participant;
import com.vz.backend.business.dto.ecabinet.ParticipantMeetingDto;
//import com.vz.backend.core.dto.UserBasicDto;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface IParticipantRepository extends IRepository<Participant> {

	@Query("SELECT COUNT(1) > 0 FROM Participant p WHERE p.meetingId =:meetingId AND p.clientId=:clientId AND p.active=TRUE "
			+ " AND p.objId =:userId AND p.role = 'HOST' AND p.type != 'ORG'")
	boolean isUserIdIsOwnerMeeting(Long userId, Long meetingId, Long clientId);

	List<Participant> findByMeetingIdAndActiveTrue(Long meetingId);
	
	@Query("SELECT p.objId FROM Participant p WHERE p.meetingId =:meetingId AND p.clientId=:clientId AND p.active=TRUE "
			+ " AND p.role = 'HOST' AND p.type != 'ORG'")
	Long getHostByMeetingId(Long meetingId, Long clientId);

	Participant findByObjIdAndMeetingIdAndActiveTrue(Long userId, Long meetingId);
	
	@Query("SELECT NEW com.vz.backend.business.dto.ecabinet.ParticipantMeetingDto(p.meetingId, u.id, u.fullName, p.role) "
			+ " FROM Participant p JOIN User u ON u.id = p.objId "
			+ " WHERE p.clientId =:clientId AND p.active=TRUE AND p.meetingId IN (:meetingIds) AND p.type != 'ORG' GROUP BY p.meetingId, u.id, u.fullName, p.role")
	List<ParticipantMeetingDto> getUserByMeetingIds(List<Long> meetingIds, Long clientId);

	List<Participant> findByClientIdAndMeetingIdAndActiveTrue(Long clientId, Long meetingId);

	List<Participant> findByClientIdAndMeetingIdAndType(Long clientId, Long meetingId, ReceiveTypeEnum type);

	@Query("SELECT DISTINCT (p.groupId) FROM Participant p WHERE p.clientId = :clientId AND p.meetingId = :meetingId and p.type = 'GROUP'")
	Long getGroupIdByMeetingId(Long clientId, Long meetingId);

	Participant findByClientIdAndMeetingIdAndTypeAndRole(Long clientId, Long meetingId, ReceiveTypeEnum group,
			RoleEnum host);

	Participant findByClientIdAndObjIdAndMeetingIdAndActiveTrue(Long clientId, Long objId, Long meetingId);

	Participant findByClientIdAndMeetingIdAndObjIdAndTypeAndActiveTrue(Long clientId, Long meetingId, Long objId,
			ReceiveTypeEnum type);

	Participant findByClientIdAndMeetingIdAndRole(Long clientId, Long id, RoleEnum role);

//	@Query("SELECT NEW com.vz.backend.core.dto.UserBasicDto(u.id, u.fullName, u.orgModel.name, u.positionModel.name) FROM Participant p JOIN User u ON p.objId = u.id "
//			+ "WHERE p.clientId = :clientId AND p.meetingId = :meetingId AND p.role = 'GUEST' AND p.active = 'TRUE'")
//	Page<UserBasicDto> findGuestByMeetingId(Long clientId, Long meetingId, Pageable pageable);

	@Query("SELECT DISTINCT(p.meetingId) FROM Participant p WHERE p.meetingId IN (:meetingIds) AND p.clientId=:clientId AND p.active=TRUE "
			+ " AND p.objId =:userId AND p.role = 'HOST' AND p.type != 'ORG'")
	List<Long> getByMeetingIdsAndUserIds(List<Long> meetingIds, Long userId, Long clientId);

	List<Participant> findByClientIdAndMeetingIdAndTypeInAndActiveTrue(Long clientId, Long meetingId,
			List<ReceiveTypeEnum> types);
	
	@Transactional
	@Modifying
	@Query("DELETE FROM Participant p WHERE p.id IN (:ids) AND p.clientId = :clientId")
	void deleteById(List<Long> ids, Long clientId);

}

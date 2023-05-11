package com.vz.backend.business.repository.ecabinet;

import java.util.List;

import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.vz.backend.business.domain.ecabinet.Agenda;
import com.vz.backend.core.dto.IdName;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface IAgendaRepository extends IRepository<Agenda> {

	Agenda findByIdAndClientIdAndActiveTrue(Long agendaId, Long clientId);

	@Query("SELECT NEW com.vz.backend.core.dto.IdName(a.meetingId, a.host.name) FROM Agenda a WHERE a.clientId =:clientId AND a.active=TRUE "
			+ "AND a.meetingId IN (:meetingIds) " + "GROUP BY a.meetingId, a.host.name ")
	List<IdName> getOrgHostByMeetingIds(List<Long> meetingIds, Long clientId);

	Agenda findByClientIdAndIdAndActiveTrue(Long clientId, Long agendaId);

	List<Agenda> findByClientIdAndMeetingIdAndActiveTrue(Long clientId, Long meetingId);

	@Query("SELECT DISTINCT(a.expertId) FROM Agenda a WHERE a.clientId =:clientId AND a.active=TRUE AND a.meetingId = (:meetingId) ")
	List<Long> getExpertByMeetingId(Long meetingId, Long clientId);

	@Query("SELECT a FROM Agenda a WHERE a.clientId = :clientId AND a.meetingId = :meetingId AND a.id IN (:ids)")
	List<Agenda> findExistAgendaByListIds(Long clientId, Long meetingId, List<Long> ids);
}

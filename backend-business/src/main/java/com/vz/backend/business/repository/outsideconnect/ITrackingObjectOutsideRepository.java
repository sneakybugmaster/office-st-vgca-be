package com.vz.backend.business.repository.outsideconnect;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.vz.backend.business.domain.outsideconnect.TrackingObjectOutside;
import com.vz.backend.business.dto.outsideconnect.SentDataDto;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface ITrackingObjectOutsideRepository extends IRepository<TrackingObjectOutside> {

	@Query("SELECT t FROM TrackingObjectOutside t WHERE t.objId =:objId AND t.type = :type AND t.clientId = :clientId AND t.active = TRUE ORDER BY t.createDate DESC")
	Page<TrackingObjectOutside> list(Long objId, DocumentTypeEnum type, Long clientId, Pageable page);

	@Query("SELECT NEW com.vz.backend.business.dto.outsideconnect.SentDataDto(t.outsideId, t.orgId, t.orgName) FROM TrackingObjectOutside t "
			+ " WHERE t.objId =:objId AND t.type = :type AND t.clientId = :clientId AND t.active = TRUE AND result = TRUE")
	List<SentDataDto> getSentData(Long objId, DocumentTypeEnum type, Long clientId);
	
	@Query("SELECT t FROM TrackingObjectOutside t WHERE t.objId = :objId AND t.type = :type "
			+ "AND t.clientId = :clientId AND t.active = TRUE AND t.outsideId IS NOT NULL AND t.result = TRUE AND t.objIdSender IS NOT NULL" )
	List<TrackingObjectOutside> getOutsideSendObjId(Long objId, DocumentTypeEnum type, Long clientId);
}

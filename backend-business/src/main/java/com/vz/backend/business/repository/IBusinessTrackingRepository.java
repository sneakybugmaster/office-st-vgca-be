package com.vz.backend.business.repository;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.vz.backend.business.config.FolderTypeEnum;
import com.vz.backend.business.domain.BusinessTracking;
import com.vz.backend.business.domain.BusinessTracking.BusinessTrackingType;
import com.vz.backend.business.dto.BusinessTrackingDto;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface IBusinessTrackingRepository extends IRepository<BusinessTracking>  {

	@Query("SELECT t FROM BusinessTracking t WHERE t.docId=:id AND t.type=:type")
	BusinessTracking findOne(Long id, BusinessTrackingType type);

	@Query("SELECT " + BusinessTrackingDto.CONSTRUCTOR_OPEN + "d.preview, d.numberOrSign) FROM BusinessTracking t "
			+ "INNER JOIN DocumentOut d ON t.docId=d.id AND t.type=:type "
			+ "WHERE d.createBy=:userId")
	Page<BusinessTrackingDto> listByVbDi(Long userId, BusinessTrackingType type, Pageable pageable);

	@Query("SELECT " + BusinessTrackingDto.CONSTRUCTOR_OPEN + "d.preview, d.numberArrival, d.numberOrSign) FROM BusinessTracking t "
			+ "INNER JOIN Documents d ON t.docId=d.id AND t.type=:type "
			+ "WHERE d.id IN ("
			+ "     SELECT p.docId FROM DocumentInProcess p "
			+ "     WHERE p.toUser=:userId OR p.frUser=:userId"
			+ ")")
	Page<BusinessTrackingDto> listByVbDen(Long userId, BusinessTrackingType type, Pageable pageable);

	@Query("SELECT " + BusinessTrackingDto.CONSTRUCTOR_OPEN + "h.title, h.fileCode) FROM BusinessTracking t "
			+ "INNER JOIN HsFolder h ON t.docId=h.id AND t.type=:type "
			+ "INNER JOIN User u on h.createBy=u.id "
			+ "WHERE (h.folderType=:canhan AND h.createBy=:userId) "
			+ "OR (h.folderType=:coquan AND u.org=:orgId)")
	Page<BusinessTrackingDto> listByHoSo(FolderTypeEnum canhan, FolderTypeEnum coquan, Long userId, Long orgId,
			BusinessTrackingType type, Pageable pageable);

}

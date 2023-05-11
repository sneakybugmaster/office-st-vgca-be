package com.vz.backend.business.repository.hstl;

import java.util.List;

import org.springframework.data.jpa.repository.Query;

import com.vz.backend.business.config.HsFolderTrackingEnum;
import com.vz.backend.business.domain.hstl.HsFolderTracking;
import com.vz.backend.business.dto.hstl.FolderTrackDto;
import com.vz.backend.core.repository.IRepository;

public interface IHsFolderTrackingRepository extends IRepository<HsFolderTracking>{

	HsFolderTracking findByFolderIdAndStatusAndClientId(Long id, HsFolderTrackingEnum transferPb, Long clientId);

	@Query("SELECT new com.vz.backend.business.dto.hstl.FolderTrackDto(u.fullName, t.createDate, t.comment, t.status) "
			+ "FROM HsFolderTracking t INNER JOIN User u ON u.id = t.createBy "
			+ "WHERE t.folderId = :folderId AND t.clientId = :clientId "
			+ "ORDER BY t.id ASC")
	List<FolderTrackDto> findByFolderId(Long folderId, Long clientId);

}

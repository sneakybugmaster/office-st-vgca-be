package com.vz.backend.business.repository.hstl;

import java.util.List;

import org.springframework.data.jpa.repository.Query;

import com.vz.backend.business.config.HsFolderProcessEnum;
import com.vz.backend.business.domain.hstl.HsFolderProcess;
import com.vz.backend.core.repository.IRepository;

public interface IHsFolderProcessRepository extends IRepository<HsFolderProcess>{
	@Query("SELECT count(*) > 0 FROM HsFolderProcess p WHERE p.folderId = :folderId "
			+ "AND (p.fromUserId = :userId OR p.toUserId = :userId OR p.toOrgId = :orgId) "
			+ "AND p.clientId = :clientId AND p.active is true")
	boolean checkPermissionByProcess(Long folderId, Long userId, Long orgId, Long clientId);
	
	HsFolderProcess findFirstByFolderIdAndToUserIdAndStatusInAndActiveAndClientIdOrderByIdDesc(Long folderId,
			Long userId, List<HsFolderProcessEnum> status, boolean active, Long clientId);

	HsFolderProcess findFirstByFolderIdAndToOrgIdAndStatusInAndActiveAndClientIdOrderByIdDesc(Long folderId, Long orgId,
			List<HsFolderProcessEnum> status, boolean active, Long clientId);
	
	HsFolderProcess findFirstByFolderIdAndStatusInAndActiveAndClientIdOrderByIdDesc(Long id,
			List<HsFolderProcessEnum> status, boolean active, Long clientId);

	HsFolderProcess findFirstByFolderIdAndToUserIdAndClientIdAndActiveTrue(Long folderId, Long userId, Long clientId);
	
	HsFolderProcess findFirstByFolderIdAndClientIdAndActiveTrue(Long folderId, Long clientId);
}

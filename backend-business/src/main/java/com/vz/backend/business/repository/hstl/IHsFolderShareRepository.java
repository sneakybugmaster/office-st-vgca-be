package com.vz.backend.business.repository.hstl;

import java.util.List;

import javax.transaction.Transactional;

import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.vz.backend.business.domain.hstl.HsFolderShare;
import com.vz.backend.business.dto.hstl.UserWithOrgAndPositionDto;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface IHsFolderShareRepository extends IRepository<HsFolderShare> {

	@Modifying
	@Transactional
	@Query("DELETE FROM HsFolderShare fs "
			+ "WHERE ((:folderId is null AND fs.folderId is null) OR (:folderId != null AND fs.folderId = :folderId)) "
			+ "AND fs.userId != (SELECT f.createBy FROM HsFolder f WHERE (:folderId != null AND f.id = :folderId)) AND fs.clientId = :clientId")
	void deleteByFolderId(Long folderId, Long clientId);

	@Query("SELECT new com.vz.backend.business.dto.hstl.UserWithOrgAndPositionDto(fs.user.id, fs.user.fullName, fs.user.orgModel.name, fs.user.positionModel.name, fs.permission) "
			+ "FROM HsFolderShare fs INNER JOIN HsFolder f ON f.id = fs.folderId "
			+ "WHERE fs.userId != f.createBy AND fs.folderId = :folderId "
			+ "AND fs.clientId = :clientId AND fs.active is true")
	List<UserWithOrgAndPositionDto> getListShare(Long folderId, Long clientId);
	
	List<HsFolderShare> findByFolderIdAndClientIdAndActiveTrue(Long folderId, Long clientId);

}

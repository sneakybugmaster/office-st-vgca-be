package com.vz.backend.business.repository.hstl;

import java.util.List;

import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.vz.backend.business.config.HsFolderStatusEnum;
import com.vz.backend.business.domain.hstl.HsFolderFile;
import com.vz.backend.business.dto.hstl.IconDetailDto;
import com.vz.backend.business.dto.hstl.ecm.FolderAttachmentDto;
import com.vz.backend.business.dto.hstl.export.ContentDoc;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface IHsFolderFileRepository extends IRepository<HsFolderFile> {

	@Query("SELECT new com.vz.backend.business.dto.hstl.IconDetailDto(ff.id, ff.fileName, ff.createDate, ff.fileType) "
			+ "FROM HsFolderFile ff "
			+ "LEFT JOIN HsFolderProcess p ON p.folderId = ff.folderId AND p.active is true "
			//			+ "LEFT JOIN HsFolderShare fs ON fs.folderId is null "
			+ "WHERE ff.folderId is null AND (ff.createBy = :userId OR p.toUserId =:userId OR p.fromUserId =:userId) "
			//			+ "OR (ff.createBy != :userId AND fs.userId = :userId)) "
			+ "AND ff.clientId = :clientId AND (:active is null OR ff.active = :active)")
	List<IconDetailDto> getListDetailByRoot(Long userId, Long clientId, Boolean active);

	@Query("SELECT new com.vz.backend.business.dto.hstl.IconDetailDto(ff.id, ff.fileName, ff.createDate, ff.fileType) "
			+ "FROM HsFolderFile ff "
			+ "WHERE ff.folderId = :folderId AND (:status is null OR ff.folder.status = :status) "
			+ "AND ff.clientId = :clientId AND (:active is null OR ff.active = :active)"
			//			+ "AND (ff.createBy =:userId and p.id IS NULL OR p.toUserId =:userId OR p.fromUserId =:userId)"
			)
	List<IconDetailDto> getListDetailByFolderId(Long folderId, HsFolderStatusEnum status, Long clientId, Boolean active);

	@Query("SELECT NEW com.vz.backend.business.dto.hstl.export.ContentDoc(fd) "
			+ "FROM HsFolderFile fd WHERE fd.folderId=:folderId AND fd.clientId=:clientId")
	List<ContentDoc> getByFolderId(Long folderId, Long clientId);

	@Query("SELECT NEW com.vz.backend.business.dto.hstl.ecm.FolderAttachmentDto(fd, false) FROM HsFolderFile fd WHERE fd.folderId IN (:ids) AND fd.clientId=:clientId ")
	List<FolderAttachmentDto> getByFolderIds(List<Long> ids, Long clientId);
}
